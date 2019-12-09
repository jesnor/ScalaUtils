package scala_utils.grid

import scala_utils.math.old.{Point_2I, Size_2I}

import scala.collection.mutable.ArrayBuffer

class Square_grid [T] (val size : Size_2I, use_manhattan_distance : Boolean, f : Point_2I => T)
    extends Grid [Point_2I, T] {
  private val _cells = ArrayBuffer.tabulate (size.area)(i => f (Point_2I (i % size.width, i / size.width)))

  override def distance (p1 : Point_2I, p2 : Point_2I) =
    if (use_manhattan_distance) p1 manhattan_distance p2 else p1 abs_distance p2

  override def apply (p : Point_2I) =
    if (p.x < 0 || p.x >= size.width || p.y < 0 || p.y >= size.height)
      None
    else
      Option (unsafe_get (p))

  private def unsafe_get (p : Point_2I) = _cells (p.x + p.y * size.width)
  override def cells = size.row_major_points map unsafe_get
  override def positions = size.row_major_points
}

case class Square_grid_factory (size : Size_2I, use_manhattan_distance : Boolean) extends Grid_factory [Point_2I] {
  override def apply [T] (f : Point_2I => T) = new Square_grid [T](size, use_manhattan_distance, f)
}
