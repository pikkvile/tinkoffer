package tinkoff

import org.joda.time.format.DateTimeFormat

import scala.math.BigDecimal.RoundingMode

object Formats {

  val moneyScale = 2
  val moneyRounding = RoundingMode.HALF_UP
  val dateFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZZ")
}