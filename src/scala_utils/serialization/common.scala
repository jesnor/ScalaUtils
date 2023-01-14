package scala_utils.serialization

import scala_utils.math.{Point_2, VPoint_2F, VPoint_2I}
import scala_utils.utils.Of

object common {
  // ------- Boolean --------

  implicit def boolean_reader [B] (implicit r: Reader [B, Byte]): Reader [B, Boolean] =
    (b: B) => r.read (b) != 0

  implicit def boolean_writer [B] (implicit w: Writer [B, Byte]): Writer [B, Boolean] =
    (b: B, t: Boolean) => w.write (b, if (t) 1 else 0)



  // ------- Char --------

  implicit def char_reader [B] (implicit r: Reader [B, Short]): Reader [B, Char] =
    (b: B) => r.read (b).toChar

  implicit def char_writer [B] (implicit w: Writer [B, Short]): Writer [B, Char] =
    (b: B, v: Char) => w.write (b, v.toShort)



  // ------- Array --------

  implicit def array_reader [B, T] (implicit r: Reader [B, T], ir: Reader [B, Int]): Reader [B, Array Of T] =
    (b: B) => {
      val a = new Array [T](ir.read (b))

      for (i <- a.indices)
        a (i) = r.read (b)

      a
    }

  implicit def array_writer [B, T] (implicit w: Writer [B, T], iw: Writer [B, Int]): Writer [B, Array Of T] =
    (b: B, t: Array [T]) => {
      iw.write (b, t.length)

      for (v <- t)
        w.write (b, v)
    }


  // ------- String --------

  implicit def utf8_string_reader [B] (implicit r: Reader [B, Array Of Byte]): Reader [B, String] =
    (b: B) => new String (r.read (b), "UTF-8")

  implicit def utf8_string_writer [B] (implicit w: Writer [B, Array Of Byte]): Writer [B, String] =
    (b: B, t: String) => w.write (b, t.getBytes ("UTF-8"))

  implicit def utf8_string_reader [B] (implicit r: Reader [B, Array Of Byte]): Reader [B, String] =
    (b: B) => new String (r.read (b), "UTF-8")

  implicit def utf8_string_writer [B] (implicit w: Writer [B, Array Of Byte]): Writer [B, String] =
    (b: B, t: String) => w.write (b, t.getBytes ("UTF-8"))



  // ------- Point_2 --------

  implicit def point_2_reader [B, T] (implicit r: Reader [B, T]): Reader [B, Point_2 [T]] =
    (b: B) => Point_2 [T](r.read (b), r.read (b))

  implicit def point_2_writer [B, T] (implicit w: Writer [B, T]): Writer [B, Point_2 [T]] =
    (b: B, v: Point_2 [T]) => {
      w.write (b, v.x)
      w.write (b, v.y)
    }



  // ------- VPoint_2F --------

  implicit def vpoint_2F_reader [B] (implicit r: Reader [B, Long]): Reader [B, VPoint_2F] =
    (b: B) => VPoint_2F (r.read (b))

  implicit def vpoint_2F_writer [B] (implicit w: Writer [B, Long]): Writer [B, VPoint_2F] =
    (b: B, v: VPoint_2F) => w.write (b, v.value)



  // ------- VPoint_2I --------

  implicit def vpoint_2I_reader [B] (implicit r: Reader [B, Long]): Reader [B, VPoint_2I] =
    (b: B) => VPoint_2I (r.read (b))

  implicit def vpoint_2I_writer [B] (implicit w: Writer [B, Long]): Writer [B, VPoint_2I] =
    (b: B, v: VPoint_2I) => w.write (b, v.value)
}
