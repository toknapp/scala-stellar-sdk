package stellar.sdk.op

import org.stellar.sdk.xdr.BumpSequenceOp
import org.stellar.sdk.xdr.Operation.OperationBody
import stellar.sdk.{PublicKey, PublicKeyOps}

import scala.util.Try

/**
  * Bump sequence bumps forward the sequence number of the source account of the operation, invalidating any
  * as yet unpublished transactions with lower sequence numbers.
  *
  * @param bumpTo if greater than the source account’s sequence number, the account’s sequence number is updated to this value
  *               otherwise ...
  * @see [[https://www.stellar.org/developers/guides/concepts/list-of-operations.html#bump-sequence operation doc]]
  */
case class BumpSequenceOperation(bumpTo: Long, sourceAccount: Option[PublicKeyOps] = None) extends Operation {
  override def toOperationBody: OperationBody = ???
}

object BumpSequenceOperation {
  def from(op: BumpSequenceOp, source: Option[PublicKey]): Try[BumpSequenceOperation] = Try {
    BumpSequenceOperation(
      bumpTo = op.getBumpTo.getSequenceNumber.getInt64,
      sourceAccount = source
    )
  }
}
