package scala_utils.math

import scala_utils.utils.utils.Of

case class Point_2 [@specialized (Int, Float, Double) T] (x : T, y : T)

object Point_2 {
  class Point_2_arithmetic [@specialized (Int, Float, Double) T]
  (implicit arith : Arithmetic [T]) extends Arithmetic [Point_2 [T]] {
    import Arithmetic.Implicits._

    override def plus (a : Point_2 [T], b : Point_2 [T]) = Point_2 (a.x + b.x, a.y + b.y)
    override def minus (a : Point_2 [T], b : Point_2 [T]) = Point_2 (a.x - b.x, a.y - b.y)
    override def times (a : Point_2 [T], b : Point_2 [T]) = Point_2 (a.x * b.x, a.y * b.y)
    override def negate (a : Point_2 [T]) = Point_2 (-a.x, -a.y)
    override def zero = Point_2 (arith.zero, arith.zero)
    override def one = Point_2 (arith.one, arith.one)
    override def abs (a : Point_2 [T]) = Point_2 (a.x.abs, a.y.abs)
    override def min_elems (a : Point_2 [T], b : Point_2 [T]) = Point_2 (a.x.min_elems (b.x), a.y.min_elems (b.y))
    override def max_elems (a : Point_2 [T], b : Point_2 [T]) = Point_2 (a.x.max_elems (b.x), a.y.max_elems (b.y))
  }

  implicit def point_2_arithmetic [@specialized (Int, Float, Double) T : Arithmetic] : Point_2_arithmetic [T] =
    new Point_2_arithmetic

  class Point_2_arith_ops [@specialized (Int, Float, Double) T] (p : Point_2 [T])(implicit arith : Arithmetic [T]) {
    import Arithmetic.Implicits._

    def add_x (v : T) : Point_2 [T] = Point_2 (p.x + v, p.y)
    def add_y (v : T) : Point_2 [T] = Point_2 (p.x, p.y + v)
    def sub_x (v : T) : Point_2 [T] = Point_2 (p.x - v, p.y)
    def sub_y (v : T) : Point_2 [T] = Point_2 (p.x, p.y - v)
    def area : T = p.x * p.y
    def manhattan_distance (b : Point_2 [T]) : T = (p.x - b.x).abs + (p.y - b.y).abs
    def max_xy_distance (b : Point_2 [T]) : T = (p.x - b.x).abs.max_elems ((p.y - b.y).abs)
  }

  implicit def point_2_arith_ops [@specialized (Int, Float, Double) T : Arithmetic] (p : Point_2 [T]) :
  Point_2_arith_ops [T] = new Point_2_arith_ops (p)

  class Point_2_numeric [@specialized (Int, Float, Double) T] (p : Point_2 [T])(implicit arith : Numeric [T]) {
    import Numeric.Implicits._

    def to_int = Point_2 (p.x.toInt, p.y.toInt)
    def to_float = Point_2 (p.x.toFloat, p.y.toFloat)
  }

  implicit def point_2_numeric [@specialized (Int, Float, Double) T : Numeric] (p : Point_2 [T]) : Point_2_numeric [T] =
    new Point_2_numeric (p)

  class Point_2F_arithmetic (a : Point_2F) extends Arithmetic.Ops (a) {
    def + (v : Float) = Point_2 (a.x + v, a.y + v)
    def - (v : Float) = Point_2 (a.x - v, a.y - v)
    def * (v : Float) = Point_2 (a.x * v, a.y * v)
    def / (v : Float) = Point_2 (a.x / v, a.y / v)
    def / (b : Point_2F) = Point_2 (a.x / b.x, a.y / b.y)
  }

  implicit def point_2F_arithmetic (p : Point_2F) : Point_2F_arithmetic = new Point_2F_arithmetic (p)

  class Point_2I_arithmetic (a : Point_2I) extends Arithmetic.Ops (a) {
    def + (v : Int) = Point_2 (a.x + v, a.y + v)
    def - (v : Int) = Point_2 (a.x - v, a.y - v)
    def * (v : Int) = Point_2 (a.x * v, a.y * v)
    def / (v : Int) = Point_2 (a.x / v, a.y / v)
    def / (b : Point_2I) = Point_2 (a.x / b.x, a.y / b.y)

    def row_major_points : Seq Of Point_2I = for (y <- 0 until a.y; x <- 0 until a.x) yield Point_2 (x, y)
  }

  implicit def point_2I_arithmetic (p : Point_2I) : Point_2I_arithmetic = new Point_2I_arithmetic (p)

  class Point_2_partial_ordering [T] (implicit o : PartialOrdering [T]) extends PartialOrdering [Point_2 [T]] {
    override def tryCompare (a : Point_2 [T], b : Point_2 [T]) : Option [Int] =
      o.tryCompare (a.x, b.x).flatMap (v1 =>
        o.tryCompare (a.y, b.y).flatMap (v2 => if (v1 == v2) Option (v1) else Option.empty))

    override def lteq (a : Point_2 [T], b : Point_2 [T]) = tryCompare (a, b).exists (c => c <= 0)
  }

  implicit def point_2_partial_ordering [T : PartialOrdering] : PartialOrdering [Point_2 [T]] =
    new Point_2_partial_ordering

  implicit object Point_2I_partial_ordering extends PartialOrdering [Point_2I] {
    override def tryCompare (a : Point_2I, b : Point_2I) : Option [Int] =
      if (a.x < b.x) {
        if (a.y < b.y) Some (-1) else None
      }
      else if (a.x > b.x) {
        if (a.y > b.y) Some (1) else None
      }
      else if (a.y == b.y) Some (0) else None

    override def lteq (a : Point_2I, b : Point_2I) = a.x <= b.x && a.y <= b.y
  }

  implicit object Point_2F_partial_ordering extends PartialOrdering [Point_2F] {
    override def tryCompare (a : Point_2F, b : Point_2F) : Option [Int] = {
      val o = implicitly [Ordering [Float]]

      o.tryCompare (a.x, b.x).flatMap (v1 =>
        o.tryCompare (a.y, b.y).flatMap (v2 => if (v1 == v2) Option (v1) else Option.empty))
    }

    override def lteq (a : Point_2F, b : Point_2F) = tryCompare (a, b).exists (c => c <= 0)
  }

  def min_max_elems [T : Arithmetic] (s : Iterable [Point_2 [T]]) : Option [(Point_2 [T], Point_2 [T])] = {
    import Arithmetic.Implicits._

    for (
      h <- s.headOption
    ) yield s.tail.foldLeft ((h, h))((m, v) => (m._1.min_elems (v), m._2.max_elems (v)))
  }

  def apply [@specialized (Int, Float, Double) T] (v : T) : Point_2 [T] = Point_2 (v, v)
}

object Point_2I {
  val zero = Point_2 (0)
  val one = Point_2 (1)
  val minus_one = Point_2 (-1)

  def apply (x : Int, y : Int) = Point_2 (x, y)
}

object Point_2F {
  val zero = Point_2 (0.0f)
  val one = Point_2 (1.0f)
  val minus_one = Point_2 (-1.0f)

  def apply (x : Float, y : Float) = Point_2 (x, y)
}
