package tinkoff

import org.scalatest.{FlatSpec, Matchers}

class NotificationSpec extends FlatSpec with Matchers {

  "NotificationRequest" should "be parsed" in {
    val req = NotificationRequest.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">\n " +
      "<tinkoff:NotificationRequest>\n <tinkoff:operation>CONFIRMED</tinkoff:operation>\n <tinkoff:merchantId>myshop" +
      "</tinkoff:merchantId>\n <tinkoff:orderId>102</tinkoff:orderId>\n <tinkoff:invoiceId>10525</tinkoff:invoiceId>\n " +
      "<tinkoff:amount>295.99</tinkoff:amount>\n </tinkoff:NotificationRequest>\n" +
      "<tinkoff:sign>4f1b833350b2964f7401f0d399d4531c5d7c8718ddd3db21a12e4df2cd50ff5f</tinkoff:sign>\n</tinkoff:invoicing>",
    "sdfJKjf84nf0@#sdfkjmd")
    req.isSuccess should be (true)
    req.foreach(nr => {
      nr.shopId should be (None)
      nr.operation should be ("CONFIRMED")
      nr.amount should be (BigDecimal(295.99))
      nr.invoiceId should be (10525)
      nr.orderId should be ("102")
    })
  }

  "NotificationRequest" should "not be parsed due to bad sign" in {
    val req = NotificationRequest.unpack("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/request\">\n " +
      "<tinkoff:NotificationRequest>\n <tinkoff:operation>CONFIRMED</tinkoff:operation>\n <tinkoff:merchantId>myshop" +
      "</tinkoff:merchantId>\n <tinkoff:orderId>102</tinkoff:orderId>\n <tinkoff:invoiceId>10525</tinkoff:invoiceId>\n " +
      "<tinkoff:amount>295.99</tinkoff:amount>\n </tinkoff:NotificationRequest>\n" +
      "<tinkoff:sign>4f1b833350b2964f7402f0d399d4531c5d7c8718ddd3db21a12e4df2cd50ff5f</tinkoff:sign>\n</tinkoff:invoicing>",
      "sdfJKjf84nf0@#sdfkjmd")
    req.isFailure should be (true)
  }

  "Success NotificationResponse" should "be packed" in {
    val xmlRes = NotificationResponse("myshop", 100523)
    xmlRes.pack() should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">" +
      "<tinkoff:NotificationResponse><tinkoff:merchantId>myshop</tinkoff:merchantId><tinkoff:invoiceId>100523" +
      "</tinkoff:invoiceId></tinkoff:NotificationResponse></tinkoff:invoicing>")
  }

  "Error NotificationResponse" should "be packed" in {
    val xmlRes = NotificationResponse("myshop", 100523, TError(1, "some error"))
    xmlRes.pack() should be ("<tinkoff:invoicing xmlns:tinkoff=\"http://tinkoff.ru/invoicing/response\">" +
      "<tinkoff:NotificationResponse><tinkoff:merchantId>myshop</tinkoff:merchantId><tinkoff:invoiceId>100523" +
      "</tinkoff:invoiceId><tinkoff:error><tinkoff:code>1</tinkoff:code><tinkoff:description>" +
      "some error</tinkoff:description></tinkoff:error></tinkoff:NotificationResponse></tinkoff:invoicing>")
  }

}
