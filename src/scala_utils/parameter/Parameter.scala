package scala_utils.parameter

import scala_utils.math.Set
import scala_utils.utils.Of
import scala_utils.utils.observables.{CObservable_var, Value_change}

class Parameter [T] (val group : Parameter_group,
                     val name : String,
                     val default : T,
                     val valid_values : Set Of T) extends CObservable_var (default) {
  def value_string (v : T) : String = v match {
    case _: Float | _: Double => v.formatted("%.2f")
    case _ => v.toString
  }

  override protected def updated (vc : Value_change Of T) = {
    super.updated (vc)
    group.param_changed (Parameter_change (this, vc))
  }
}
