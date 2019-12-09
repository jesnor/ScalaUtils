package scala_utils.serialization

trait Serializer [B, T] {
  def reader : Reader [B, T]
  def writer : Writer [B, T]
}
