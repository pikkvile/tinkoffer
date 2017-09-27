package tinkoff

import scala.xml.NodeSeq

case class GetStatusRequest(merchantId: String, invoiceId: Int, shopId: Option[String]) {
  def pack(password: String): String = xml.Utility.trim(
    <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/request">
      <tinkoff:GetStatusRequest>
        <tinkoff:merchantId>{merchantId}</tinkoff:merchantId>
        {shopId.map(sid => <tinkoff:shopId>{sid}</tinkoff:shopId>).getOrElse(NodeSeq.Empty)}
        <tinkoff:invoiceId>{invoiceId}</tinkoff:invoiceId>
      </tinkoff:GetStatusRequest>
      <tinkoff:sign>{Utils.sign(asKeyValueSeq :+ ("password", password))}</tinkoff:sign>
    </tinkoff:invoicing>).toString

  private def asKeyValueSeq: Seq[(String, String)] = List(
    ("merchantId", merchantId),
    ("invoiceId", invoiceId.toString),
  ) ++ shopId.map(sid => ("shopId", sid))
}

object GetStatusRequest {
  def apply(merchantId: String, invoiceId: Int): GetStatusRequest =
    new GetStatusRequest(merchantId, invoiceId, None)
  def apply(merchantId: String, invoiceId: Int, shopId: String): GetStatusRequest =
    new GetStatusRequest(merchantId, invoiceId, Some(shopId))
}

case class GetStatusResponse(merchantId: String, invoiceId: Int, status: String, shopId: Option[String])

object GetStatusResponse {
  def unpack(xmlResponse: String): Either[TError, GetStatusResponse] = {
    val res = xml.XML.loadString(xmlResponse)
    TError.unpack(res) match {
      case Some(error) => Left(error)
      case None => Right(extractResponse(res))
    }
  }
  private def extractResponse(res: NodeSeq): GetStatusResponse = GetStatusResponse(
    res \ "GetStatusResponse" \ "merchantId" text,
    (res \ "GetStatusResponse" \ "invoiceId" text).toInt,
    res \ "GetStatusResponse" \ "status" text,
    res \ "GetStatusResponse" \ "shopId" map (_.text) headOption
  )
}