package scala_utils.math

import scala.reflect.ClassTag

class Array_2D [T : ClassTag] (val size : Point_2I, f : Point_2I => T) {
  private val cells = Array.tabulate (size.area)(i => f (Point_2 (i % size.x, i / size.x)))

  def is_inside (p : Point_2I) : Boolean = is_inside (p.x, p.y)
  def is_inside (x : Int, y : Int) : Boolean = x >= 0 && y >= 0 && x < size.x && y < size.y

  def update (x : Int, y : Int, value : T) : Boolean =
    if (is_inside (x, y)) {
      cells (x + y * size.x) = value
      true
    }
    else
      false

  def apply (x : Int, y : Int) : Option [T] = if (is_inside (x, y)) Some (unsafe_get (x, y)) else None
  def update (p : Point_2I, value : T) : Boolean = this (p.x, p.y) = value
  def apply (p : Point_2I) : Option [T] = this (p.x, p.y)

  def unsafe_get (p : Point_2I) : T = unsafe_get (p.x, p.y)
  def unsafe_get (x : Int, y : Int) : T = cells (x + y * size.x)
}
