package scala_utils.parameter

import scala_utils.math.{Range, Range_double, Set}
import scala_utils.utils.Of
import scala_utils.utils.observables.CObservable

import scala.collection.mutable.ArrayBuffer

class Parameter_group (val name : String) extends CObservable [Parameter_change [_]] {
  var parent : Option [Parameter_group] = None
  val groups = new ArrayBuffer [Parameter_group]()
  val parameters = new ArrayBuffer [Parameter [_]]()

  def add_percent_param (name : String, default : Double) : Parameter Of Double =
    add_percent_param (name, default, Range (0.0, 1.0))

  def add_percent_param (name : String, default : Double, range : Range_double) : Parameter Of Double =
    add_param (new Parameter (this, name, range.clamp (default), range) {
      override def value_string (v : Double) = (v * 100).round + "%"
    })

  def add_boolean_param (name : String, default : Boolean = false) =
    add_param (new Parameter (this, name, default, Set.booleans))

  def add_param [T] (name : String, default : T, valid_values : Set [T]) : Parameter Of T =
    add_param (new Parameter (this, name, default, valid_values))

  def add_param [T] (p : Parameter [T]) : Parameter Of T = {
    parameters += p
    p
  }

  def add_group (group : Parameter_group) = {
    group.parent = Some (this)
    groups += group
    group
  }

  def param_changed (pc : Parameter_change [_]) : Unit = {
    fire_change (pc)
    parent.foreach (_ param_changed pc)
  }
}
