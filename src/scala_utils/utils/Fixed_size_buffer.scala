package scala_utils.utils

import scala.reflect.ClassTag

class Fixed_size_buffer [T >: Null : ClassTag] (_size : Int) {
  private val array = new Array [T](_size)
  private var size = 0

  def += (value : T) : Boolean = {
    for (i <- 0 until size)
      if (array (i) == null) {
        array (i) = value
        return true
      }

    if (size == array.length) false else {
      array (size) = value
      size += 1
      true
    }
  }

  def foreach (f : T => Boolean) = {
    for (i <- 0 until size) {
      val v = array (i)

      if (v != null && !f (v)) {
        array (i) = null

        if (i == size)
          size -= 1
      }
    }
  }
}
