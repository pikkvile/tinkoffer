package tinkoff

import tinkoff.Formats.{moneyRounding, moneyScale}

import scala.util.{Failure, Success, Try}
import scala.xml.NodeSeq

case class NotificationRequest(operation: String,
                               merchantId: String,
                               shopId: Option[String],
                               orderId: String,
                               invoiceId: Int,
                               amount: BigDecimal) {

  private def asKeyValueSeq: Seq[(String, String)] = List(
    ("operation", operation),
    ("merchantId", merchantId),
    ("orderId", orderId),
    ("invoiceId", invoiceId.toString),
    ("amount", amount.setScale(moneyScale, moneyRounding).toString)
  ) ++ shopId.map(sid => ("shopId", sid))
}

object NotificationRequest {

  def unpack(xmlRequest: String, password: String): Try[NotificationRequest] = {
    val nodeSeq = xml.XML.loadString(xmlRequest)
    val req = extractRequest(nodeSeq)
    val sign = (nodeSeq \ "sign").text
    if (sign == Utils.sign(req.asKeyValueSeq :+ ("password", password)))
      Success(req)
    else
      Failure(new SecurityException(s"Sign $sign is invalid for $req"))
  }

  private def extractRequest(req: NodeSeq): NotificationRequest = {
    NotificationRequest(req \ "NotificationRequest" \ "operation" text,
      req \ "NotificationRequest" \ "merchantId" text,
      req \ "NotificationRequest" \ "shopId" map (_.text) headOption,
      req \ "NotificationRequest" \ "orderId" text,
      (req \ "NotificationRequest" \ "invoiceId" text).toInt,
      BigDecimal(req \ "NotificationRequest" \ "amount" text))
  }

}

case class NotificationResponse(merchantId: String,
                                shopId: Option[String],
                                invoiceId: Int,
                                error: Option[TError]) {
  def pack(): String = xml.Utility.trim(
    <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/response">
      <tinkoff:NotificationResponse>
        <tinkoff:merchantId>{merchantId}</tinkoff:merchantId>
        {shopId.map(sid => <tinkoff:shopId>{sid}</tinkoff:shopId>).getOrElse(NodeSeq.Empty)}
        <tinkoff:invoiceId>{invoiceId}</tinkoff:invoiceId>
        {error.map(e => <tinkoff:error>
          <tinkoff:code>{e.code}</tinkoff:code>
          <tinkoff:description>{e.description}</tinkoff:description>
        </tinkoff:error>).getOrElse(NodeSeq.Empty)}
      </tinkoff:NotificationResponse>
    </tinkoff:invoicing>).toString
}

object NotificationResponse {
  def apply(merchantId: String, invoiceId: Int): NotificationResponse =
    new NotificationResponse(merchantId, None, invoiceId, None)
  def apply(merchantId: String, invoiceId: Int, error: TError): NotificationResponse =
    new NotificationResponse(merchantId, None, invoiceId, Some(error))
}
