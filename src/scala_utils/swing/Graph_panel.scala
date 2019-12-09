package scala_utils.swing

import java.awt.Color

import scala_utils.math.Arithmetic._
import scala_utils.math.Point_2._
import scala_utils.math.{Point_2, Point_2F}
import scala_utils.swing.utils._

import scala.Numeric._
import scala.swing.{Graphics2D, Panel}

class Graph_panel extends Panel {
  type Data_point = Point_2F

  trait Data_set {
    def data : Iterable [Data_point]
    def color : Color
  }

  case class Simple_data_set (data : Iterable [Data_point], color : Color) extends Data_set {
    val min_max = Point_2.min_max_elems (data)
  }

  private var data_sets : Iterable [Data_set] = Iterable.empty
  private val inset = 4
  private var data_window : (Data_point, Data_point) = (Point_2F.zero, Point_2F.zero)

  override protected def paintComponent (g : Graphics2D) : Unit = {
    super.paintComponent (g)

    for {
      ds <- data_sets
      head <- ds.data.headOption
    } {
      g setColor ds.color

      def to_screen (p : Data_point) = {
        val a = size.to_point - inset * 2
        val c = (p - data_window._1) / (data_window._2 - data_window._1)
        val b = (c * a.to_float).to_int
        Point_2 (inset) + Point_2 (b.x, a.y - b.y)
      }

      var lsp = to_screen (head)

      for {
        p <- ds.data
        sp = to_screen (p)
      } {
        g.drawLine (lsp.x, lsp.y, sp.x, sp.y)
        lsp = sp
      }
    }
  }

  def set_data (data_window : (Data_point, Data_point), data_sets : Iterable [Data_set]) : Unit = {
    this.data_window = data_window
    this.data_sets = data_sets
    repaint ()
  }

  background = new Color (20, 40, 60)
}
