package scala_utils.utils

import java.util

import scala_utils.utils.utils.Of

object observables {
  type Observer [-T] = T => Unit

  trait Observable [T] {
    def add_observer (observer : Observer Of T)
    def remove_observer (observer : Observer Of T)
  }

  class Observer_manager {
    private val observers = new util.HashSet [Observer Of _]

    def add_observer [T] (obs : Observable [T], observer : Observer Of T) = {
      obs.add_observer (observer)
      observers.add (observer)
    }

    def remove_observer [T] (obs : Observable [T], observer : Observer Of T) = {
      obs.remove_observer (observer)
      observers.remove (obs)
    }
  }

  case class Value_change [T] (old_value : T, new_value : T)

  trait Value [T] {
    def apply () : T
  }

  abstract class ObservableValue [T] extends Protected_weak_set [Observer Of (Value_change Of T)] with Value [T] with Observable [Value_change Of T] {
    override def add_observer (observer : Observer Of (Value_change Of T)) = add (observer)
    override def remove_observer (observer : Observer Of (Value_change Of T)) = remove (observer)

    protected def fireChange (oldValue : T, newValue : T) = {
      val vc = Value_change (oldValue, newValue)
      foreach (obs => obs (vc))
    }
  }

  trait Var [T] {
    def apply (value : T)
  }

  abstract class ObservableVar [T] extends ObservableValue [T] with Var [T] {}

  def observableVar [T] (v : T) = new ObservableVar [T] {
    private var value = v
    override def apply () : T = value

    override def apply (v : T) : Unit = {
      if (v != value) {
        val oldValue = value
        value = v
        fireChange (oldValue, v)
      }
    }
  }
}
