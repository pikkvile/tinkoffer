package tinkoff

import org.scalatest.{FlatSpec, Matchers}

class CancelSpec extends FlatSpec with Matchers {

  "CancelRequest" should "be created and packed" in {
    val req = CancelRequest("myshop", 12345, BigDecimal(123.34), "shop-1")
    req.pack("mypass") should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">" +
      "<tinkoff:CancelRequest><tinkoff:merchantId>myshop</tinkoff:merchantId><tinkoff:shopId>shop-1</tinkoff:shopId>" +
      "<tinkoff:invoiceId>12345</tinkoff:invoiceId><tinkoff:amount>123.34</tinkoff:amount></tinkoff:CancelRequest>" +
      "<tinkoff:sign>74cdc0f8d81e8cd6ec8ef78e4fcb37a19c6e6de40c8bb2e58d893fb4ffbd998b</tinkoff:sign></tinkoff:invoicing>")
  }

  "CancelRequest" should "without shopId be created and packed" in {
    val req = CancelRequest("myshop", 12345, BigDecimal(123.34))
    req.pack("mypass") should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">" +
      "<tinkoff:CancelRequest><tinkoff:merchantId>myshop</tinkoff:merchantId>" +
      "<tinkoff:invoiceId>12345</tinkoff:invoiceId><tinkoff:amount>123.34</tinkoff:amount></tinkoff:CancelRequest>" +
      "<tinkoff:sign>edc897d9c57ecee75632319d0144199081f6180d558486ae8c9774de13189ef6</tinkoff:sign></tinkoff:invoicing>")
  }

  "CancelResponse" should "be parsed" in {
    val res = CancelResponse.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">\n " +
      "<tinkoff:CancelResponse>\n <tinkoff:merchantId>myshop</tinkoff:merchantId>\n <tinkoff:invoiceId>100523" +
      "</tinkoff:invoiceId>\n </tinkoff:CancelResponse>\n</tinkoff:invoicing>")
    res.isRight should be (true)
    res.right.get.merchantId should be ("myshop")
    res.right.get.invoiceId should be (100523)
    res.right.get.shopId should be (None)
  }

  "TError" should "be parsed" in {
    val res = CancelResponse.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">\n " +
      "<tinkoff:CancelResponse><tinkoff:error>\n <tinkoff:code>456</tinkoff:code>\n <tinkoff:description>sdlkjfh" +
      "</tinkoff:description>\n </tinkoff:error></tinkoff:CancelResponse>\n</tinkoff:invoicing>")
    res.isLeft should be (true)
    res.left.get.code should be (456)
    res.left.get.description should be ("sdlkjfh")
  }
}
