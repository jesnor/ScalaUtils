package scala_utils.serialization

trait Writer [B, T] {
  def write (b : B, t : T) : Unit
}
