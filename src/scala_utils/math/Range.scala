package scala_utils.math

case class Range [@specialized (Int, Float, Double) T] (min : T, max : T) {
  def apply (v : T)(implicit o : PartialOrdering [T]) = o.lteq (min, v) && o.gteq (max, v)
  def include (v : T)(implicit o : Arithmetic [T]) : Range [T] = Range (o.min_elems (min, v), o.max_elems (max, v))
  def clamp (v : T)(implicit o : Arithmetic [T]) : T = o.max_elems (min, o.min_elems (max, v))
  def size (implicit o : Arithmetic [T]) : T = o.minus (max, min)
  def to_double (implicit o : Numeric [T]) : Range_double = Range (o.toDouble (min), o.toDouble (max))
  def to_int (implicit o : Numeric [T]) : Range_int = Range (o.toInt (min), o.toInt (max))

  def & (r : Range [T])(implicit o : Arithmetic [T]) : Range [T] =
    Range (o.max_elems (min, r.min), o.min_elems (max, r.max))

  def | (r : Range [T])(implicit o : Arithmetic [T]) = Range (o.min_elems (min, r.min), o.max_elems (max, r.max))
}

object Range {
  class Range_arith_ops [T] (a : T)(implicit ar : Arithmetic [T]) {
    def range_zero = Range (ar.zero, a)
  }

  implicit def range_arith_ops [T : Arithmetic] (a : T) : Range_arith_ops [T] = new Range_arith_ops (a)

  def apply [@specialized (Int, Float, Double) T] (v : T) : Range [T] = Range (v, v)
}

object Range_int {
  val zero = Range (0, 0)
  val one = Range (1, 1)
}

object Range_float {
  val zero = Range (0f, 0f)
  val one = Range (1f, 1f)
}
