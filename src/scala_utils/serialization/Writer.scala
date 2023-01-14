package scala_utils.serialization

trait Writer [B, T] {
  def write (b: B, t: T)
}

object Writer {
  implicit case class Writer_ops [B] (b: B) extends AnyVal {
    def write [T] (v: T)(implicit w: Writer [B, T]): Unit = w.write (b, v)
  }
}
