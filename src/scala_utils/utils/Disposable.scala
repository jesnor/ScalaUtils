package scala_utils.utils

trait Disposable {
  def dispose () : Unit
}

object Disposable {
  val nop: Disposable = () => {}
}
