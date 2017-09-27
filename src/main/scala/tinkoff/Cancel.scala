package tinkoff

import tinkoff.Formats.{moneyRounding, moneyScale}

import scala.xml.NodeSeq

case class CancelRequest(merchantId: String, invoiceId: Int, amount: BigDecimal, shopId: Option[String]) {
  def pack(password: String): String = xml.Utility.trim(
    <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/request">
      <tinkoff:CancelRequest>
        <tinkoff:merchantId>{merchantId}</tinkoff:merchantId>
        {shopId.map(sid => <tinkoff:shopId>{sid}</tinkoff:shopId>).getOrElse(NodeSeq.Empty)}
        <tinkoff:invoiceId>{invoiceId}</tinkoff:invoiceId>
        <tinkoff:amount>{amount.setScale(moneyScale, moneyRounding).toString}</tinkoff:amount>
      </tinkoff:CancelRequest>
      <tinkoff:sign>{Utils.sign(asKeyValueSeq :+ ("password", password))}</tinkoff:sign>
    </tinkoff:invoicing>).toString

  private def asKeyValueSeq: Seq[(String, String)] = List(
    ("merchantId", merchantId),
    ("invoiceId", invoiceId.toString),
    ("amount", amount.setScale(moneyScale, moneyRounding).toString)
  ) ++ shopId.map(sid => ("shopId", sid))
}

object CancelRequest {
  def apply(merchantId: String, invoiceId: Int, amount: BigDecimal): CancelRequest =
    new CancelRequest(merchantId, invoiceId, amount, None)
  def apply(merchantId: String, invoiceId: Int, amount: BigDecimal, shopId: String): CancelRequest =
    new CancelRequest(merchantId, invoiceId, amount, Some(shopId))
}

case class CancelResponse(merchantId: String, invoiceId: Int, shopId: Option[String])

object CancelResponse {
  def unpack(xmlResponse: String): Either[TError, CancelResponse] = {
    val res = xml.XML.loadString(xmlResponse)
    TError.unpack(res) match {
      case Some(error) => Left(error)
      case None => Right(extractResponse(res))
    }
  }
  private def extractResponse(res: NodeSeq): CancelResponse = CancelResponse(
    res \ "CancelResponse" \ "merchantId" text,
    (res \ "CancelResponse" \ "invoiceId" text).toInt,
    res \ "CancelResponse" \ "shopId" map (_.text) headOption
  )
}