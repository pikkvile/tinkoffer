package tinkoff

import org.scalatest.{FlatSpec, Matchers}

class GetStatusSpec extends FlatSpec with Matchers {

  "GetStatusRequest" should "be created and packed" in {
    val req = GetStatusRequest("myshop", 12345, "shop-1")
    req.pack("mypass") should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">" +
      "<tinkoff:GetStatusRequest><tinkoff:merchantId>myshop</tinkoff:merchantId><tinkoff:shopId>shop-1</tinkoff:shopId>" +
      "<tinkoff:invoiceId>12345</tinkoff:invoiceId></tinkoff:GetStatusRequest>" +
      "<tinkoff:sign>cc0f969f1111cde084c76993cc6989980e01d6bb5999953a1bbb970a3b6a2878</tinkoff:sign></tinkoff:invoicing>")
  }

  "GetStatusRequest" should "without shopId be created and packed" in {
    val req = GetStatusRequest("myshop", 12345)
    req.pack("mypass") should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">" +
      "<tinkoff:GetStatusRequest><tinkoff:merchantId>myshop</tinkoff:merchantId>" +
      "<tinkoff:invoiceId>12345</tinkoff:invoiceId></tinkoff:GetStatusRequest>" +
      "<tinkoff:sign>421bc0ae11ecbde1d180005e5dc5b691c9b50edb162a719e0eaf5dd74a2f097e</tinkoff:sign></tinkoff:invoicing>")
  }

  "GetStatusResponse" should "be parsed" in {
    val res = GetStatusResponse.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">\n " +
      "<tinkoff:GetStatusResponse>\n <tinkoff:merchantId>myshop</tinkoff:merchantId>\n <tinkoff:invoiceId>100523" +
      "</tinkoff:invoiceId><tinkoff:status>Created</tinkoff:status></tinkoff:GetStatusResponse>\n</tinkoff:invoicing>")
    res.isRight should be (true)
    res.right.get.merchantId should be ("myshop")
    res.right.get.invoiceId should be (100523)
    res.right.get.shopId should be (None)
    res.right.get.status should be ("Created")
  }

  "TError" should "be parsed" in {
    val res = GetStatusResponse.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">\n " +
      "<tinkoff:GetStatusResponse><tinkoff:error>\n <tinkoff:code>456</tinkoff:code>\n <tinkoff:description>sdlkjfh" +
      "</tinkoff:description>\n </tinkoff:error></tinkoff:GetStatusResponse>\n</tinkoff:invoicing>")
    res.isLeft should be (true)
    res.left.get.code should be (456)
    res.left.get.description should be ("sdlkjfh")
  }
}
