package scala_utils.utils

import java.lang.ref.WeakReference

/**
 * Memory efficient set for weak references
 *
 * Mainly used as base class for observables
 */
class Protected_weak_set [T] {
  private var entries : Any = _

  protected def add (value : T) : T = {
    val w = new WeakReference (value)

    entries = entries match {
      case null => w
      case wr : WeakReference [T] => if (wr.get == null) w else Array (w, wr, null, null)

      case a : Array [WeakReference [T]] =>
        // Find empty entry
        for (i <- a.indices) {
          if (a (i) == null || a (i).get == null) {
            a (i) = w
            return value
          }
        }

        val newEntries = new Array [WeakReference [T]](a.length * 2)
        System.arraycopy (a, 0, newEntries, 0, a.length)
        newEntries (a.length) = w
        newEntries
    }

    value
  }

  protected def remove (value : T) : Boolean = entries match {
    case null =>
      entries = null
      false

    case wr : WeakReference [T] => wr.get match {
      case null =>
        entries = null
        false

      case `value` =>
        entries = null
        true

      case _ =>
        entries = wr
        false
    }

    case a : Array [WeakReference [T]] =>
      var count = 0
      var lastIndex = -1
      var rv = false

      for (i <- a.indices) {
        if (a (i) != null) {
          val v = a (i).get

          if (v == null || v == value) {
            a (i) = null
            rv ||= v == value
          } else {
            count += 1
            lastIndex = i
          }
        }
      }

      count match {
        case 0 => entries = null
        case 1 => entries = a (lastIndex)

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

          entries = newEntries

        case _ =>
      }

      rv
  }

  protected def foreach (action : T => Unit) : Unit = {
    def doWith (wr : WeakReference [T]) = {
      val value = wr.get

      if (value != null)
        action (value)
    }

    entries match {
      case null =>
      case wr : WeakReference [T] => doWith (wr)
      case a : Array [WeakReference [T]] => a.foreach (doWith)
    }
  }
}
