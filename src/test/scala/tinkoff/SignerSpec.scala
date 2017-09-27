package tinkoff

import org.scalatest.{FlatSpec, Matchers}
import Utils._

class SignerSpec extends FlatSpec with Matchers {

  "Signer" should "sign" in {
    val keyValueSeq = List(
      ("merchantId", "myshop"),
      ("orderId", "102"),
      ("amount", "295.99"),
      ("endDate", "2016-08-31T12:28:00+03:00"),
      ("shortDesc", "Оплата заказа №102"),
      ("fullDesc", "Оплата заказа №102 на сайте myshop.ru"),
      ("returnURL", "http://myshop.ru/invoice.html"),
      ("email", "a@test.ru"),
      ("password", "sdfJKjf84nf0@#sdfkjmd")
    )
    sign(keyValueSeq) should be ("7216f614df1096e20d48645f1d14f49a275b684e03ca48b44656cbb9ecf2884e")
  }
}
