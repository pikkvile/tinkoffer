package tinkoff

import scala.xml.NodeSeq

case class TError(code: Int, description: String)

object TError {
  def unpack(res: NodeSeq): Option[TError] = {
    res \ "failure" match {
      case NodeSeq.Empty =>
        res \ "_" \ "error" match {
          case NodeSeq.Empty => None
          case e:NodeSeq => Some(TError((e \ "code").text.toInt, e \ "description" text))
        }
      case f:NodeSeq => Some(TError((f \ "error-code" text).toInt, f \ "error-description" text))
    }
  }
}