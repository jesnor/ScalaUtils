package scala_utils.serialization

object little_endian {
  def write_short [B] (b: B, value: Short)(implicit w: Writer [B, Byte]): Unit = {
    w.write (b, (value & 0xFF).toByte)
    w.write (b, (value >> 8).toByte)
  }

  def write_int [B] (b: B, value: Int)(implicit w: Writer [B, Byte]): Unit = {
    write_short (b, (value & 0xFFFF).toShort)
    write_short (b, (value >> 16).toShort)
  }

  def write_long [B] (b: B, value: Long)(implicit w: Writer [B, Byte]): Unit = {
    write_int (b, (value & 0xFFFFFFFF).toInt)
    write_int (b, (value >> 32).toInt)
  }

  def read_short [B] (b: B)(implicit r: Reader [B, Byte]): Short = (r.read (b) | (r.read (b) << 8)).toShort
  def read_int [B] (b: B)(implicit r: Reader [B, Byte]): Int = read_short (b) | (read_short (b) << 16)
  def read_long [B] (b: B)(implicit r: Reader [B, Byte]): Long = read_int (b).toLong | (read_int (b).toLong << 32)

  implicit def le_short_writer [B] (implicit w: Writer [B, Byte]): Writer [B, Short] =
    (b: B, t: Short) => write_short (b, t)

  implicit def le_int_writer [B] (implicit w: Writer [B, Byte]): Writer [B, Int] = (b: B, t: Int) => write_int (b, t)

  implicit def le_long_writer [B] (implicit w: Writer [B, Byte]): Writer [B, Long] =
    (b: B, t: Long) => write_long (b, t)

  implicit def le_short_reader [B] (implicit r: Reader [B, Byte]): Reader [B, Short] = (b: B) => read_short (b)

  implicit def le_int_reader [B] (implicit r: Reader [B, Byte]): Reader [B, Int] = (b: B) => read_int (b)

  implicit def le_long_reader [B] (implicit r: Reader [B, Byte]): Reader [B, Long] = (b: B) => read_long (b)
}
