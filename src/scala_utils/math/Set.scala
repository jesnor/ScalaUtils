package scala_utils.math

trait Set [T] extends (T => Boolean)
trait Iterable_set [T] extends Set [T] with scala.collection.immutable.Set [T]

object Set {
  val booleans = new Iterable_set [Boolean] {
    override def incl (elem : Boolean) = this

    override def excl (elem : Boolean) =
      if (elem) scala.collection.immutable.Set (false) else scala.collection.immutable.Set (true)

    override def contains (elem : Boolean) = true
    override def iterator = Seq (false, true).iterator
  }
}
