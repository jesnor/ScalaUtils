package scala_utils.parameter

import scala_utils.utils.observables.Value_change

case class Parameter_change [T] (parameter : Parameter [T], change : Value_change [T])
