package scala_utils.math.old

import scala.math._

case class Point_2I (x : Int, y : Int) {
  def manhattan_distance (p : Point_2I) = abs (x - p.x) + abs (y - p.y)
  def abs_distance (p : Point_2I) = (x - p.x).abs max (y - p.y).abs
  def - (p : Point_2I) = Size_2I (x - p.x, y - p.y)
}

object Point_2I {
  val zero = Point_2I (0, 0)
  val one = Point_2I (1, 1)
}
