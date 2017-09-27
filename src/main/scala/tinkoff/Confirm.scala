package tinkoff

import tinkoff.Formats.{moneyRounding, moneyScale}

import scala.xml.NodeSeq

case class ConfirmRequest(merchantId: String, invoiceId: Int, amount: BigDecimal, shopId: Option[String]) {
  def pack(password: String): String = xml.Utility.trim(
    <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/request">
      <tinkoff:ConfirmRequest>
        <tinkoff:merchantId>{merchantId}</tinkoff:merchantId>
        {shopId.map(sid => <tinkoff:shopId>{sid}</tinkoff:shopId>).getOrElse(NodeSeq.Empty)}
        <tinkoff:invoiceId>{invoiceId}</tinkoff:invoiceId>
        <tinkoff:amount>{amount.setScale(moneyScale, moneyRounding).toString}</tinkoff:amount>
      </tinkoff:ConfirmRequest>
      <tinkoff:sign>{Utils.sign(asKeyValueSeq :+ ("password", password))}</tinkoff:sign>
    </tinkoff:invoicing>).toString

  private def asKeyValueSeq: Seq[(String, String)] = List(
    ("merchantId", merchantId),
    ("invoiceId", invoiceId.toString),
    ("amount", amount.setScale(moneyScale, moneyRounding).toString)
  ) ++ shopId.map(sid => ("shopId", sid))
}

object ConfirmRequest {
  def apply(merchantId: String, invoiceId: Int, amount: BigDecimal): ConfirmRequest =
    new ConfirmRequest(merchantId, invoiceId, amount, None)
  def apply(merchantId: String, invoiceId: Int, amount: BigDecimal, shopId: String): ConfirmRequest =
    new ConfirmRequest(merchantId, invoiceId, amount, Some(shopId))
}

case class ConfirmResponse(merchantId: String, invoiceId: Int, amount: BigDecimal, shopId: Option[String])

object ConfirmResponse {
  def unpack(xmlResponse: String): Either[TError, ConfirmResponse] = {
    val res = xml.XML.loadString(xmlResponse)
    TError.unpack(res) match {
      case Some(error) => Left(error)
      case None => Right(extractResponse(res))
    }
  }
  private def extractResponse(res: NodeSeq): ConfirmResponse = ConfirmResponse(
    res \ "ConfirmResponse" \ "merchantId" text,
    (res \ "ConfirmResponse" \ "invoiceId" text).toInt,
    BigDecimal(res \ "ConfirmResponse" \ "amount" text),
    res \ "ConfirmResponse" \ "shopId" map (_.text) headOption
  )
}