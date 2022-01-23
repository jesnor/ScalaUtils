package scala_utils.utils

object observables {
  type Observer [-T] = T => Unit

  trait Observable [T] {
    def add_observer (observer: Observer Of T): Observer Of T
    def remove_observer (observer: Observer Of T): Unit

    def now_and_on_change (f: => Unit): Observer Of T = {
      f
      add_observer (_ => f)
    }
  }

  class CObservable [T] extends Protected_weak_set [Observer Of T] with Observable [T] {
    override def add_observer (observer: Observer Of T) = add (observer)
    override def remove_observer (observer: Observer Of T) = remove (observer)
    protected def fire_change (v: T): Unit = foreach_listener (_ (v))
  }

  case class Value_change [+T] (old_value: T, new_value: T)
  trait Observable_value [T] extends Value [T] with Observable [Value_change Of T]
  trait Observable_var [T] extends Observable_value [T] with Var [T]

  abstract class CObservable_value [T] extends CObservable [Value_change Of T] with Observable_value [T]

  class CObservable_var [T] (v: T) extends CObservable_value [T] with Observable_var [T] {
    private var value: T = v
    override def apply (): T = value

    override def update (v: T): Unit = {
      if (v != value) {
        val oldValue = value
        value = v
        val change = Value_change (oldValue, v)
        fire_change (change)
        updated (change)
      }
    }

    protected def updated (vc: Value_change [T]) = {}
  }

  def observable_var [T] (v: T): Observable_var [T] = new CObservable_var [T](v)

  def const_observable_value [T] (v: T): Observable_value [T] = new Observable_value [T] {
    override def apply (): T = v
    override def add_observer (observer: Observer Of (Value_change Of T)) = observer
    override def remove_observer (observer: Observer Of (Value_change Of T)) = {}
  }
}
