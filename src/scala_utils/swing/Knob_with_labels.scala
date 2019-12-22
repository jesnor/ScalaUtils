package scala_utils.swing

import scala_utils.math.Range_double
import scala_utils.swing.utils.label

import scala.swing.GridBagPanel

class Knob_with_labels (title : String,
                        range : Range_double,
                        value : Double,
                        action : Double => Unit,
                        exp : Double = 1,
                        to_string : Double => String = _.round.toString) extends GridBagPanel {
  val title_label = label (title)
  val value_label = label (to_string (value))

  val knob = new Knob (range, value, v => {
    value_label.text = to_string (v)
    action (v)
  }, exp)

  val c = new Constraints
  add (title_label, c)
  c.gridy = 1
  add (knob, c)
  c.gridy = 2
  add (value_label, c)

  def set_value (v : Double) = {
    value_label.text = to_string (v)
    knob.set_value_without_callback (v)
  }

  /*    p.layout (title_label).grid = (0, 0)
      p.layout (value_label).grid = (1, 0)
      p.layout (knob).grid = (1, 0)
      p.layout (knob).gridwidth = 2
    */
}
