package scala_utils.math

case class Range_2 [@specialized (Int, Float, Double) T] (min : T, max : T) {
  def apply (v : T)(implicit o : PartialOrdering [T]) = o.lteq (min, v) && o.gteq (max, v)
  def include (v : T)(implicit o : Arithmetic [T]) : Range_2 [T] = Range_2 (o.min_elems (min, v), o.max_elems (max, v))

  def & (r : Range_2 [T])(implicit o : Arithmetic [T]) : Range_2 [T] =
    Range_2 (o.max_elems (min, r.min), o.min_elems (max, r.max))

  def | (r : Range_2 [T])(implicit o : Arithmetic [T]) = Range_2 (o.min_elems (min, r.min), o.max_elems (max, r.max))
}

object Range_2 {
  class Range_arith_ops [T] (a : T)(implicit ar : Arithmetic [T]) {
    def range_zero = Range_2 (ar.zero, a)
  }

  implicit def range_arith_ops [T : Arithmetic] (a : T) : Range_arith_ops [T] = new Range_arith_ops (a)

  def apply [@specialized (Int, Float, Double) T] (v : T) : Range_2 [T] = Range_2 (v, v)
}

object Range_2I {
  val zero = Range_2 (0, 0)
  val one = Range_2 (1, 1)
}

object Range_2F {
  val zero = Range_2 (0f, 0f)
  val one = Range_2 (1f, 1f)
}
