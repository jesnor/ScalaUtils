package scala_utils.serialization

trait Reader [B, T] {
  def read (b : B) : T
}
