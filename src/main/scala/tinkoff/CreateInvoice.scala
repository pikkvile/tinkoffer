package tinkoff

import org.joda.time.DateTime
import tinkoff.Formats._
import tinkoff.Utils._

import scala.xml.NodeSeq

case class CreateInvoiceRequest(merchantId: String,
                                shop: Option[Shop] = None,
                                orderId: String,
                                amount: BigDecimal,
                                endDate: DateTime,
                                shortDesc: String,
                                fullDesc: String,
                                returnUrl: String,
                                extFields: Option[Map[String, String]] = None) {

  def pack(password: String): String = xml.Utility.trim (
    <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/request">
      <tinkoff:CreateInvoiceRequest>
        <tinkoff:merchantId>{merchantId}</tinkoff:merchantId>
        {shopXml}
        <tinkoff:orderId>{orderId}</tinkoff:orderId>
        <tinkoff:amount>{amount.setScale(moneyScale, moneyRounding).toString}</tinkoff:amount>
        <tinkoff:endDate>{endDate.toString(dateFormat)}</tinkoff:endDate>
        <tinkoff:shortDesc>{shortDesc}</tinkoff:shortDesc>
        <tinkoff:fullDesc>{fullDesc}</tinkoff:fullDesc>
        <tinkoff:returnURL>{returnUrl}</tinkoff:returnURL>
        {extFieldsXml}
      </tinkoff:CreateInvoiceRequest>
      <tinkoff:sign>{sign(asKeyValueSeq :+ ("password", password))}</tinkoff:sign>
    </tinkoff:invoicing>).toString

  private def extFieldsXml: NodeSeq =
    extFields.map(efs => <tinkoff:extFields>
      {efs.map {case (k, v) =>
        <tinkoff:extField key={k}>{v}</tinkoff:extField>}
      }</tinkoff:extFields>)
      .getOrElse(NodeSeq.Empty)

  private def shopXml: NodeSeq = shop.map(s =>
    <tinkoff:shopId>{s.id}</tinkoff:shopId> ++ <tinkoff:shopName>{s.name}</tinkoff:shopName>)
    .getOrElse(NodeSeq.Empty)

  private def asKeyValueSeq: Seq[(String, String)] = List(
    ("merchantId", merchantId),
    ("orderId", orderId),
    ("amount", amount.setScale(moneyScale, moneyRounding).toString),
    ("endDate", endDate.toString(dateFormat)),
    ("shortDesc", shortDesc),
    ("fullDesc", fullDesc),
    ("returnUrl", returnUrl),
  ) ++ shop.map(_.asKeyValueSeq).getOrElse(Seq.empty) ++ extFields.map(efs => efs.toSeq).getOrElse(Seq.empty)
}

case class CreateInvoiceResponse(merchantId: String,
                                 shopId: Option[String],
                                 orderId: String,
                                 invoiceId: Int,
                                 paymentUrl: String)

object CreateInvoiceResponse {

  def unpack(xmlResponse: String): Either[TError, CreateInvoiceResponse] = {
    val res = xml.XML.loadString(xmlResponse)
    TError.unpack(res) match {
      case Some(error) => Left(error)
      case None => Right(extractResponse(res))
    }
  }

  private def extractResponse(res: NodeSeq): CreateInvoiceResponse =
    CreateInvoiceResponse(res \ "CreateInvoiceResponse" \ "merchantId" text,
      res \ "CreateInvoiceResponse" \ "shopId" map (_.text) headOption,
      res \ "CreateInvoiceResponse" \ "orderId" text,
      (res \ "CreateInvoiceResponse" \ "invoiceId" text).toInt,
      res \ "CreateInvoiceResponse" \ "paymentUrl" text)
}

case class Shop(id: String, name: String) {
  def asKeyValueSeq: Seq[(String, String)] = List(("shopId", id), ("shopName", name))
}