package scala_utils.math

case class VPoint_2F (value: Long) extends AnyVal {
  def x: Float = java.lang.Float.intBitsToFloat ((value & 0xFFFFFFFF).toInt)
  def y: Float = java.lang.Float.intBitsToFloat ((value >> 32).toInt)

  def + (p: VPoint_2F): VPoint_2F = VPoint_2F (x + p.x, y + p.y)
  def - (p: VPoint_2F): VPoint_2F = VPoint_2F (x - p.x, y - p.y)
  def * (p: VPoint_2F): VPoint_2F = VPoint_2F (x * p.x, y * p.y)

  def + (a: Float): VPoint_2F = VPoint_2F (x + a, y + a)
  def - (a: Float): VPoint_2F = VPoint_2F (x - a, y - a)
  def * (a: Float): VPoint_2F = VPoint_2F (x * a, y * a)

  def <= (p: VPoint_2F): Boolean = x <= p.x && y <= p.y
  def <= (a: Float): Boolean = x <= a && y <= a

  def flipped: VPoint_2F = VPoint_2F (y, x)
  def abs: VPoint_2F = VPoint_2F (Math.abs (x), Math.abs (y))
  def recip: VPoint_2F = VPoint_2F (1.0f / x, 1.0f / y)

  override def toString = "Point2F(" + x + ", " + y + ")"
}

object VPoint_2F {
  def apply (x: Float, y: Float): VPoint_2F =
    VPoint_2F (java.lang.Float.floatToIntBits (x) | (java.lang.Float.floatToIntBits (y).toLong << 32))

  def unapply (p: VPoint_2F): (Float, Float) = (p.x, p.y)
}
