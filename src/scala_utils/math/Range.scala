package scala_utils.math

trait Range [T] extends Set [T] {
  def min: T
  def max: T
}

trait Iterable_range [T] extends Range [T] with Iterable_set [T]

object Range {
  private case class CRange_int (min: Int, max: Int) extends Iterable_range [Int] {
    override def incl (elem: Int): scala.collection.immutable.Set [Int] = if (this (elem)) this else toSet + elem
    override def excl (elem: Int): scala.collection.immutable.Set [Int] = if (this (elem)) toSet - elem else this
    override def contains (elem: Int): Boolean = elem >= min && elem <= max
    override def iterator: Iterator [Int] = (min to max).iterator
  }

  private case class CRange_float (min: Float, max: Float) extends Range [Float] {
    override def apply (v: Float): Boolean = v >= min && v <= max
  }

  private case class CRange_double (min: Double, max: Double) extends Range [Double] {
    override def apply (v: Double): Boolean = v >= min && v <= max
  }

  private case class CRange [T: PartialOrdering] (min: T, max: T) extends Range [T] {
    override def apply (v: T): Boolean = {
      val o = implicitly [PartialOrdering [T]]
      o.lteq (min, v) && o.gteq (max, v)
    }
  }

  trait Range_factory [T] extends ((T, T) => Range [T])
  implicit val range_int_factory: Range_factory [Int] = (v1: Int, v2: Int) => CRange_int (v1, v2)
  implicit val range_float_factory: Range_factory [Float] = (v1: Float, v2: Float) => CRange_float (v1, v2)
  implicit val range_double_factory: Range_factory [Double] = (v1: Double, v2: Double) => CRange_double (v1, v2)
  implicit def range_factory [T: PartialOrdering]: Range_factory [T] = (v1: T, v2: T) => CRange (v1, v2)

  class Range_arith_ops [T] (a: T)(implicit ar: Arithmetic [T], rf: Range_factory [T]) {
    def range_zero: Range [T] = rf (ar.zero, a)
  }

  implicit def range_arith_ops [T: Arithmetic : Range_factory] (a: T): Range_arith_ops [T] =
    new Range_arith_ops [T](a)

  class Range_arith_ops2 [T] (r: Range [T])(implicit ar: Arithmetic [T], rf: Range_factory [T]) {
    def include (v: T): Range [T] = rf (ar.min_elems (r.min, v), ar.max_elems (r.max, v))
    def clamp (v: T): T = ar.max_elems (r.min, ar.min_elems (r.max, v))
    def size: T = ar.minus (r.max, r.min)
    def & (r: Range [T]): Range [T] = rf (ar.max_elems (r.min, r.min), ar.min_elems (r.max, r.max))
    def | (r2: Range [T]): Range [T] = rf (ar.min_elems (r.min, r2.min), ar.max_elems (r.max, r2.max))
  }

  implicit def range_arith_ops2 [T: Arithmetic : Range_factory] (r: Range [T]): Range_arith_ops2 [T] =
    new Range_arith_ops2 (r)

  class Range_numeric_ops [T] (r: Range [T])(implicit o: Numeric [T]) {
    def to_double: Range_double = Range (o.toDouble (r.min), o.toDouble (r.max))
    def to_int: Range_int = Range (o.toInt (r.min), o.toInt (r.max))
  }

  implicit def range_numeric_ops [T: Numeric] (r: Range [T]): Range_numeric_ops [T] =
    new Range_numeric_ops (r)

  def apply [T] (v: T)(implicit rf: Range_factory [T]): Range [T] = rf (v, v)
  def apply [T] (min: T, max: T)(implicit rf: Range_factory [T]): Range [T] = rf (min, max)
  def apply (v: Int): Iterable_range [Int] = CRange_int (v, v)
  def apply (min: Int, max: Int): Iterable_range [Int] = CRange_int (min, max)

  implicit class Range_double_ops (val r: Range [Double]) extends AnyVal {
    def lerp (f: Double): Double = interpolate (f, r.min, r.max)
  }
}

object Range_int {
  val zero = Range (0, 0)
  val one = Range (1, 1)
  val all = Range (Int.MinValue, Int.MaxValue)
}

object Range_float {
  val zero = Range (0f, 0f)
  val one = Range (1f, 1f)
}

object Range_double {
  val zero = Range (0.0, 0.0)
  val one = Range (1.0, 1.0)
}
