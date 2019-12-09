package scala_utils.grid

trait Grid_factory [Pos] {
  def apply [T] (f : Pos => T) : Grid [Pos, T]
}
