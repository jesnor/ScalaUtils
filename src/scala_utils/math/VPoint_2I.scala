package scala_utils.math

case class VPoint_2I (value: Long) extends AnyVal {
  def x: Int = (value & 0xFFFFFFFF).toInt
  def y: Int = (value >> 32).toInt
  def & (m: Int): VPoint_2I = VPoint_2I (x & m, y & m)
  def >> (s: Int): VPoint_2I = VPoint_2I (x >> s, y >> s)

  def + (p: VPoint_2I): VPoint_2I = VPoint_2I (x + p.x, y + p.y)
  def - (p: VPoint_2I): VPoint_2I = VPoint_2I (x - p.x, y - p.y)
  def * (p: VPoint_2I): VPoint_2I = VPoint_2I (x * p.x, y * p.y)

  def + (a: Int): VPoint_2I = VPoint_2I (x + a, y + a)
  def - (a: Int): VPoint_2I = VPoint_2I (x - a, y - a)
  def * (a: Int): VPoint_2I = VPoint_2I (x * a, y * a)

  def + (p: VPoint_2F): VPoint_2F = VPoint_2F (x + p.x, y + p.y)
  def - (p: VPoint_2F): VPoint_2F = VPoint_2F (x - p.x, y - p.y)
  def * (p: VPoint_2F): VPoint_2F = VPoint_2F (x * p.x, y * p.y)

  def <= (p: VPoint_2I): Boolean = x <= p.x && y <= p.y
  def <= (a: Int): Boolean = x <= a && y <= a

  def flipped: VPoint_2I = VPoint_2I (y, x)
  def abs: VPoint_2I = VPoint_2I (Math.abs (x), Math.abs (y))
  def copy (x: Int = x, y: Int = y): VPoint_2I = VPoint_2I (x, y)

  override def toString = "Point2I(" + x + ", " + y + ")"
}

object VPoint_2I {
  def apply (x: Int, y: Int): VPoint_2I = VPoint_2I ((x.toLong & 0xFFFFFFFF) | (y.toLong << 32))
}
