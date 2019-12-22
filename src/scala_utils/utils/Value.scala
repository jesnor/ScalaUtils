package scala_utils.utils

trait Value [+T] extends (() => T) {
  def apply () : T
}
