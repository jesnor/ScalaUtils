package scala_utils.serialization

trait Reader [B, T] {
  def read (b: B): T
}

object Reader {
  implicit case class Reader_ops [B] (b: B) extends AnyVal {
    def read [T] ()(implicit r: Reader [B, T]): T = r.read (b)
  }
}
