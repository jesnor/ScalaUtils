package scala_utils.swing

import java.awt.{Component, Container, Dimension, LayoutManager}

import scala_utils.math.{Point_2, Point_2F}
import scala_utils.swing.utils._

class Aspect_ratio_layout (val aspect : Float, pos_weight : Point_2F = Point_2F.zero) extends LayoutManager {
  override def addLayoutComponent (name : String, comp : Component) = {}
  override def removeLayoutComponent (comp : Component) = {}

  override def preferredLayoutSize (parent : Container) =
    parent.getComponents.headOption.map (_.getPreferredSize).getOrElse (new Dimension ())

  override def minimumLayoutSize (parent : Container) =
    parent.getComponents.headOption.map (_.getMinimumSize).getOrElse (new Dimension ())

  override def layoutContainer (parent : Container) = {
    val ps = parent.getSize.to_point
    val aw = parent.getHeight * aspect
    val w = aw.min (parent.getWidth)
    val s = Point_2 (w, w / aspect)
    val pos = (ps.to_float - s) * pos_weight

    for (c <- parent.getComponents)
      c.setBounds (pos.x.toInt, pos.y.toInt, s.x.toInt, s.y.toInt)
  }
}
