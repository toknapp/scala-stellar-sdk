package stellar.sdk.model

import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JArray, JObject}
import stellar.sdk.model.response.{NetworkInfo, ResponseParser}

/**
  * A payment path, detailing the amounts and assets needed to facilitate the payment.
  *
  * @param from the amount to be sent
  * @param to   the amount that would be received
  * @param path the intermediate asset path required to achieve this full path
  * @see [[https://www.stellar.org/developers/horizon/reference/endpoints/path-finding.html endpoint doc]]
  */
case class PaymentPath(from: Amount, to: Amount, path: Seq[Asset])

// TODO - should be an easy way to convert this response to an operation.


object PaymentPathDeserializer extends ResponseParser[PaymentPath]({ o: JObject =>
  import AmountDeserializerMethods._
  implicit val formats = DefaultFormats

  val JArray(pathJs) = o \ "path"
  PaymentPath(
    from = amount(o, assetPrefix = "source"),
    to = amount(o, assetPrefix = "destination"),
    path = pathJs.map(a => asset(o = a))
  )
})