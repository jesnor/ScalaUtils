package scala_utils.utils

trait Var [T] extends Value [T] {
  def update (value : T) : Unit
}
