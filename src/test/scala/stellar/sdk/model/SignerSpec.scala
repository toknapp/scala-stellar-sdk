package stellar.sdk.model

import org.specs2.mutable.Specification
import stellar.sdk.{ArbitraryInput, DomainMatchers}

class SignerSpec extends Specification with ArbitraryInput with DomainMatchers {

  "a signer" should {
    "serde via xdr bytes" >> prop { expected: Signer =>
      val (remaining, actual) = Signer.decode.run(expected.encode).value
      actual must beEquivalentTo(expected)
      remaining must beEmpty
    }
  }

}
