package tinkoff

import org.scalatest.{FlatSpec, Matchers}
import Utils._
import org.joda.time.DateTime

class CreateInvoiceSpec extends FlatSpec with Matchers {

  "CreateInvoiceRequest" should "be created and packed" in {
    val req = CreateInvoiceRequest("myshop", Shop("sh1", "My favourite shop"), "ord1", BigDecimal(123.34),
      new DateTime("2017-10-25"), "shDesc", "full desc", "http://someurl.com", None)
    req.pack("passwd") should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">" +
      "<tinkoff:CreateInvoiceRequest><tinkoff:merchantId>myshop</tinkoff:merchantId><tinkoff:shopId>sh1" +
      "</tinkoff:shopId><tinkoff:shopName>My favourite shop</tinkoff:shopName><tinkoff:orderId>ord1" +
      "</tinkoff:orderId><tinkoff:amount>123.34</tinkoff:amount><tinkoff:endDate>2017-10-25T00:00:00+03:00" +
      "</tinkoff:endDate><tinkoff:shortDesc>shDesc</tinkoff:shortDesc><tinkoff:fullDesc>full desc</tinkoff:fullDesc>" +
      "<tinkoff:returnURL>http://someurl.com</tinkoff:returnURL></tinkoff:CreateInvoiceRequest><tinkoff:sign>" +
      "79a7affb294cb7a5747f0e10e2b31cfe01248722bd2df19096a70c19d90a3918</tinkoff:sign></tinkoff:invoicing>")
  }

  "CreateInvoiceRequest with extra fields" should "be created and packed" in {
    val req = CreateInvoiceRequest("myshop", Shop("sh1", "My favourite shop"), "ord1", BigDecimal(123.34),
      new DateTime("2017-10-25"), "shDesc", "full desc", "http://someurl.com", Map("k1" -> "v1", "k2" -> "v2"))
    req.pack("passwd") should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">" +
      "<tinkoff:CreateInvoiceRequest><tinkoff:merchantId>myshop</tinkoff:merchantId><tinkoff:shopId>sh1" +
      "</tinkoff:shopId><tinkoff:shopName>My favourite shop</tinkoff:shopName><tinkoff:orderId>ord1</tinkoff:orderId>" +
      "<tinkoff:amount>123.34</tinkoff:amount><tinkoff:endDate>2017-10-25T00:00:00+03:00</tinkoff:endDate>" +
      "<tinkoff:shortDesc>shDesc</tinkoff:shortDesc><tinkoff:fullDesc>full desc</tinkoff:fullDesc><tinkoff:returnURL>" +
      "http://someurl.com</tinkoff:returnURL><tinkoff:extFields><tinkoff:extField key=\"k1\">v1</tinkoff:extField>" +
      "<tinkoff:extField key=\"k2\">v2</tinkoff:extField></tinkoff:extFields></tinkoff:CreateInvoiceRequest>" +
      "<tinkoff:sign>a66c276894799ce47122854b9fd2e2bddcc6801f19bd1a414c9c0b7a60bacd8c</tinkoff:sign></tinkoff:invoicing>")
  }

  "CreateInvoiceResponse" should "be parsed" in {
    val res = CreateInvoiceResponse.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">" +
      "\n <tinkoff:CreateInvoiceResponse>\n <tinkoff:merchantId>myshop</tinkoff:merchantId>\n <tinkoff:orderId>102" +
      "</tinkoff:orderId>\n <tinkoff:invoiceId>100523</tinkoff:invoiceId>\n <tinkoff:paymentUrl>" +
      "http://tinkoff.ru/invoice/1a4b546cd</tinkoff:paymentUrl>\n </tinkoff:CreateInvoiceResponse>\n</tinkoff:invoicing>")
    res.isRight should be (true)
    res.right.get.merchantId should be ("myshop")
    res.right.get.orderId should be ("102")
    res.right.get.invoiceId should be (100523)
    res.right.get.paymentUrl should be ("http://tinkoff.ru/invoice/1a4b546cd")
    res.right.get.shopId should be (None)
  }

  "CreateInvoiceResponse with shopId" should "be parsed" in {
    val res = CreateInvoiceResponse.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">" +
      "\n <tinkoff:CreateInvoiceResponse>\n <tinkoff:merchantId>myshop</tinkoff:merchantId>\n <tinkoff:orderId>102" +
      "</tinkoff:orderId>\n <tinkoff:invoiceId>100523</tinkoff:invoiceId><tinkoff:shopId>100523</tinkoff:shopId>\n <tinkoff:paymentUrl>" +
      "http://tinkoff.ru/invoice/1a4b546cd</tinkoff:paymentUrl>\n </tinkoff:CreateInvoiceResponse>\n</tinkoff:invoicing>")
    res.right.get.shopId.get should be ("100523")
  }
}