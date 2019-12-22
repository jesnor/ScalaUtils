package scala_utils.channel

import scala_utils.serialization.{Reader, Writer}
import scala_utils.utils.Of

import scala.util.Try

trait Channel [In, Out] extends Disposable {
  def in : In
  def out : Out
  def send [T] (msg : T)(implicit w : Writer [Out, T]) = w.write (out, msg)
  def receive [T] ()(implicit r : Reader [In, T]) : Option Of (Try Of T)
}
