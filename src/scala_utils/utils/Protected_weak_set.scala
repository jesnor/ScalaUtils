package scala_utils.utils

import java.lang.ref.WeakReference

class Protected_weak_set [T] {
  private var entries : Any = null

  protected def add (value : T) : WeakReference [T] = {
    val w = new WeakReference (value)

    entries = entries match {
      case null => w
      case wr : WeakReference [T] => if (wr.get == null) w else Array (w, wr, null, null)

      case a : Array [WeakReference [T]] =>
        for (i <- 0 until a.length) {
          if (a (i) == null || a (i).get == null) {
            a (i) = w
            return w
          }
        }

        val newEntries = new Array [WeakReference [T]](a.length * 2)
        System.arraycopy (a, 0, newEntries, 0, a.length)
        newEntries (a.length) = w
        newEntries
    }

    w
  }

  protected def remove (value : T) : Unit = {
    entries = entries match {
      case null => null

      case wr : WeakReference [T] => wr.get match {
        case null | `value` => null
        case _ => wr
      }

      case a : Array [WeakReference [T]] =>
        var count = 0
        var lastIndex = -1

        for (i <- 0 until a.length) {
          if (a (i) != null) {
            val v = a (i).get

            if (v == null || v == value)
              a (i) = null
            else {
              count += 1
              lastIndex = i
            }
          }
        }

        count match {
          case 0 => null
          case 1 => a (lastIndex)

          case _ if count <= a.length / 4 =>
            val newEntries = new Array [WeakReference [T]](a.length / 2)

            if (lastIndex < newEntries.length)
              System.arraycopy (a, 0, newEntries, 0, lastIndex + 1)
            else {
              var i = 0
              var j = 0

              while (i < a.length && j < count) {
                if (a (i) != null && a (i).get != null) {
                  newEntries (j) = a (i)
                  j += 1
                }

                i += 1
              }
            }

            newEntries

          case _ => entries
        }
    }
  }

  protected def foreach (action : T => Unit) : Unit = {
    def doWith (wr : WeakReference [T]) = {
      val value = wr.get

      if (value != null)
        action (value)
    }

    entries match {
      case wr : WeakReference [T] => doWith (wr)
      case a : Array [WeakReference [T]] => a.foreach (doWith)
    }
  }
}
