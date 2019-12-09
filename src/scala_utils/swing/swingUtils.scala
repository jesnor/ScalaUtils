package scala_utils.swing

import java.awt._

object swingUtils {
  val emptyInsets = new Insets(0, 0, 0, 0)
  val standardInsets = new Insets(4, 4, 4, 4)

  def drawString(g: Graphics2D, x: Double, y: Double, xAnchor: Double, yAnchor: Double, text: String,
                 textColor: Color = null, borderColor: Color = null, backColor: Color = null,
                 insets: Insets = emptyInsets, ovalBorder: Boolean = false) = {
    val b = g.getFontMetrics.getStringBounds(text, g)
    val h = g.getFontMetrics.getAscent + g.getFontMetrics.getDescent
    val bx = x - xAnchor * (b.getWidth + insets.left + insets.right)
    val by = y - yAnchor * (h + insets.top + insets.bottom)
    val bw = b.getWidth.toInt + insets.left + insets.right - 1
    val bh = h.toInt + insets.top + insets.bottom - 1

    if (backColor != null) {
      g.setPaint(backColor)

      if (ovalBorder)
        g.fillOval(bx.toInt, by.toInt, bw, bh)
      else
        g.fillRect(bx.toInt, by.toInt, bw, bh)
    }

    if (borderColor != null) {
      g.setColor(borderColor)
      g.setStroke(new BasicStroke(1))

      if (ovalBorder)
        g.drawOval(bx.toInt, by.toInt, bw, bh)
      else
        g.drawRect(bx.toInt, by.toInt, bw, bh)
    }

    if (textColor != null)
      g.setColor(textColor)

    g.drawString(text, bx.toFloat + insets.left, by.toFloat + g.getFontMetrics.getAscent.toFloat + insets.top)
  }
}
