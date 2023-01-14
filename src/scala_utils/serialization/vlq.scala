package scala_utils.serialization

import scala_utils.utils.Of

object vlq {
  private def write_uint_part_7 [B] (b: B, v: Long)(implicit w: Writer [B, Byte]): Long = {
    val v2 = v >>> 7
    w.write (b, ((if (v2 >= 0) 0x80 else 0) | (v & 0x7F)).toByte)
    v2
  }

  private def write_uint_part_15 [B] (b: B, v: Long)(implicit w: Writer [B, Byte]): Long = {
    val v2 = v >>> 15
    little_endian.write_short (b, ((if (v2 >= 0) 0x8000 else 0) | (v & 0x7FFF)).toShort)
    v2
  }

  def write_ushort_15 [B] (b: B, value: Short)(implicit w: Writer [B, Byte]): Unit = {
    val v = write_uint_part_7 (b, value & 0x7FFF)

    if (v > 0)
      w.write (b, v.toByte)
  }

  def write_ulong [B] (b: B, value: Long)(implicit w: Writer [B, Byte]): Unit = {
    var v = write_uint_part_15 (b, value & 0x7FFFFFFF)

    while (v > 0)
      v = write_uint_part_7 (b, v)
  }

  private def read_ubyte [B] (b: B)(implicit r: Reader [B, Byte]): Int = r.read (b).toInt & 0xFF

  private def read_ushort [B] (b: B)(implicit r: Reader [B, Byte]): Int =
    little_endian.read_short (b).toInt & 0xFFFF

  def read_ushort_15 [B] (b: B)(implicit r: Reader [B, Byte]): Short = {
    val a = read_ubyte (b)

    if (a < 0x80)
      a.toShort
    else
      (a | (read_ubyte (b) << 7)).toShort
  }

  def read_ulong [B] (b: B)(implicit r: Reader [B, Byte]): Long = {
    var v: Long = read_ushort (b)
    var shift = 15

    if (v >= 0x8000) {
      var a = 0

      do {
        a = read_ubyte (b)
        v |= a.toLong << shift
        shift += 7
      } while (a >= 0x80)
    }

    v
  }

  def read_array[B, T](b: B)(implicit r: Reader[B, Array Of T]) = {
    
  }
}
