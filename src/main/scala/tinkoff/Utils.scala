package tinkoff

import java.security.MessageDigest

object Utils {

  def sign(data: Seq[(String, String)]): String =
    hash(data.sortBy(_._1).map(_._2).mkString(""))

  private def hash(s: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val bytes = s.getBytes("UTF-8")
    md.update(bytes, 0, bytes.length)
    new java.math.BigInteger(1, md.digest()).toString(16)
  }

  implicit def asSome[T](value: T): Some[T] = Some(value)
}