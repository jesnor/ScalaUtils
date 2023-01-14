package scala_utils.utils

case class VOption [T] (value: T = null) extends AnyVal with IterableOnce [T] {
  def to_option: Option [T] = Option (value)
  override def iterator: Iterator [T] = if (value == null) Iterator.empty else Iterator.apply (value)

  def isDefined: Boolean = value != null
  def nonEmpty: Boolean = isDefined
  def isEmpty: Boolean = value == null
}
