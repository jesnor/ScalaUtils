package scala_utils.math.old

import scala_utils.utils.utils.Of

case class Size_2I (width : Int, height : Int) {
  def area = width * height
  def row_major_points : Seq Of Point_2I = for (y <- 0 until height; x <- 0 until width) yield Point_2I (x, y)
}
