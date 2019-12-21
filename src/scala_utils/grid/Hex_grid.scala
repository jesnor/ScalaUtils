package scala_utils.grid

import scala_utils.math.Range._
import scala_utils.math.{Point_2, Point_2I}

import scala.collection.mutable.ArrayBuffer

class Hex_grid [T] (val size : Point_2I, f : Point_2I => T) extends Grid [Point_2I, T] {
  private val _cells = ArrayBuffer.tabulate (size.area)(i => f (Point_2 (i % size.x, i / size.x)))

  override def distance (p1 : Point_2I, p2 : Point_2I) = {
    val even_odd = (p1.y & 1) * 2 - 1
    val a = p2 - p1
    val y_add = if (a.x * even_odd > 0) 1 else 0
    val d = a.abs
    d.y + (d.x - (d.y + y_add) / 2).max (0)
  }

  override def apply (p : Point_2I) = {
    val r = size.range_zero

    if (r (p))
      Option (unsafe_get (p))
    else
      None
  }

  private def unsafe_get (p : Point_2I) = _cells (p.x + p.y * size.x)
  override def cells = size.row_major_points map unsafe_get
  override def positions = size.row_major_points
}

case class Hex_grid_factory (size : Point_2I) extends Grid_factory [Point_2I] {
  override def apply [T] (f : Point_2I => T) = new Hex_grid [T](size, f)
}
