package scala_utils.grid

import scala_utils.math.Range_int
import scala_utils.utils.Of

trait Grid [Pos, Cell] {
  def size : Pos
  def distance (p1 : Pos, p2 : Pos) : Int
  def apply (p : Pos) : Option Of Cell
  def cells : Seq Of Cell
  def positions : Seq Of Pos
  def cells_with_pos : Seq Of (Cell, Pos) = cells.zip (positions)

  def cells_within_range (p : Pos, range : Range_int) : Seq Of (Cell, Pos) =
    cells_with_pos.filter (t => range (distance (p, t._2)))
}
