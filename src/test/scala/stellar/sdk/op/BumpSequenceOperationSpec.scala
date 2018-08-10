package stellar.sdk.op

import org.json4s.NoTypeHints
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization
import org.scalacheck.Arbitrary
import org.specs2.mutable.Specification
import stellar.sdk.{ArbitraryInput, DomainMatchers}

class BumpSequenceOperationSpec extends Specification with ArbitraryInput with DomainMatchers with JsonSnippets {

  implicit val arb: Arbitrary[Transacted[BumpSequenceOperation]] = Arbitrary(genTransacted(genBumpSequenceOperation))
  implicit val formats = Serialization.formats(NoTypeHints) + TransactedOperationDeserializer // + BumpSequenceOperationDeserializer

  "bump sequence operation" should {
    "serde via xdr" >> prop { actual: BumpSequenceOperation =>
      Operation.fromXDR(actual.toXDR) must beSuccessfulTry.like {
        case expected: BumpSequenceOperation => expected must beEquivalentTo(actual)
      }
    }

    // todo - this json format needs to be confirmed when v10 is on testnet.
    "be parsed from json " >> prop { op: Transacted[BumpSequenceOperation] =>
      val doc =
        s"""
           |{
           |  "_links": {
           |    "self": {"href": "https://horizon-testnet.stellar.org/operations/10157597659137"},
           |    "transaction": {"href": "https://horizon-testnet.stellar.org/transactions/17a670bc424ff5ce3b386dbfaae9990b66a2a37b4fbe51547e8794962a3f9e6a"},
           |    "effects": {"href": "https://horizon-testnet.stellar.org/operations/10157597659137/effects"},
           |    "succeeds": {"href": "https://horizon-testnet.stellar.org/effects?order=desc\u0026cursor=10157597659137"},
           |    "precedes": {"href": "https://horizon-testnet.stellar.org/effects?order=asc\u0026cursor=10157597659137"}
           |  },
           |  "id": "${op.id}",
           |  "paging_token": "10157597659137",
           |  "source_account": "${op.operation.sourceAccount.get.accountId}",
           |  "type": "create_account",
           |  "type_i": 0,
           |  "created_at": "${formatter.format(op.createdAt)}",
           |  "transaction_hash": "${op.txnHash}",
           |  "bump_to": ${op.operation.bumpTo}
           |}
         """.stripMargin

      parse(doc).extract[Transacted[BumpSequenceOperation]] mustEqual op
    }.setGen(genTransacted(genBumpSequenceOperation.suchThat(_.sourceAccount.nonEmpty)))
  }

}
