package scala_utils.math.old

trait Range [T] extends (T => Boolean) {
  def min : T
  def max : T
}

case class Range_2I (min : Int, max : Int) extends Range [Int] {
  def apply (v : Int) = v >= min && v <= max
}

object Range_2I {
  val zero = Range_2I (0, 0)
  val one = Range_2I (1, 1)
}
