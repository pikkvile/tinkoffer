package tinkoff

import org.scalatest.{FlatSpec, Matchers}

import scala.None

class TErrorSpec extends FlatSpec with Matchers {

  "An error" should "be parsed from failure response" in {
    val err = TError.unpack(
      <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/response">
        <tinkoff:failure>
          <tinkoff:error-code>234</tinkoff:error-code>
          <tinkoff:error-description>asdsdf</tinkoff:error-description>
        </tinkoff:failure>
      </tinkoff:invoicing>)
    err.isDefined should be (true)
    err.get.code should be (234)
    err.get.description should be ("asdsdf")
  }

  "An error" should "be parsed from error response" in {
    val err = TError.unpack(
      <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/response">
        <tinkoff:SomeResponse>
          <tinkoff:error>
            <tinkoff:code>234</tinkoff:code>
            <tinkoff:description>asdsdf</tinkoff:description>
          </tinkoff:error>
        </tinkoff:SomeResponse>
      </tinkoff:invoicing>)
    err.isDefined should be (true)
    err.get.code should be (234)
    err.get.description should be ("asdsdf")
  }

  "An error" should "not be parsed from success response" in {
    val err = TError.unpack(
      <tinkoff:invoicing xmlns:tinkoff="http://tinkoff.ru/invoicing/response">
        <tinkoff:SomeResponse>
          <tinkoff:SomeField>sdsdf</tinkoff:SomeField>
        </tinkoff:SomeResponse>
      </tinkoff:invoicing>)
    err should be (None)
  }
}