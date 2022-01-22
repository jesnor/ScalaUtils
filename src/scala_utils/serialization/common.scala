package scala_utils.serialization

import scala_utils.utils.Of

object common {
  implicit def boolean_writer [B] (implicit w : Writer [B, Byte]) : Writer [B, Boolean] =
    (b : B, t : Boolean) => w.write (b, if (t) 1 else 0)

  implicit def array_writer [B, T] (implicit w : Writer [B, T], iw : Writer [B, Int]) : Writer [B, Of [Array, T]] =
    (b : B, t : Array [T]) => {
      iw.write (b, t.length)

      for (v <- t)
        w.write (b, v)
    }

  implicit def utf8_string_writer [B] (implicit w : Writer [B, Array Of Byte]) : Writer [B, String] =
    (b : B, t : String) => w.write (b, t.getBytes ("UTF-8"))
}
