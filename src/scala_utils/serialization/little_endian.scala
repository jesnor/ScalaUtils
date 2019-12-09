package scala_utils.serialization

object little_endian {
  def write_short [B] (b : B, value : Short, w : Writer [B, Byte]) = {
    w.write (b, (value & 0xFF).toByte)
    w.write (b, (value >> 8).toByte)
  }

  def write_int [B] (b : B, value : Int, w : Writer [B, Byte]) = {
    write_short (b, (value & 0xFFFF).toShort, w)
    write_short (b, (value >> 16).toShort, w)
  }

  def write_long [B] (b : B, value : Long, w : Writer [B, Byte]) = {
    write_int (b, (value & 0xFFFFFFFF).toInt, w)
    write_int (b, (value >> 32).toInt, w)
  }

  def read_short [B] (b : B, r : Reader [B, Byte]) = (r.read (b) | (r.read (b) << 8)).toShort
  def read_int [B] (b : B, r : Reader [B, Byte]) = read_short (b, r) | (read_short (b, r) << 16)
  def read_long [B] (b : B, r : Reader [B, Byte]) = read_int (b, r).toLong | (read_int (b, r).toLong << 32)

  implicit def le_short_writer [B] (implicit w : Writer [B, Byte]) = new Writer [B, Short] {
    override def write (b : B, t : Short) = write_short (b, t, w)
  }

  implicit def le_int_writer [B] (implicit w : Writer [B, Byte]) = new Writer [B, Int] {
    override def write (b : B, t : Int) = write_int (b, t, w)
  }

  implicit def le_long_writer [B] (implicit w : Writer [B, Byte]) = new Writer [B, Long] {
    override def write (b : B, t : Long) = write_long (b, t, w)
  }

  implicit def le_short_reader [B] (implicit r : Reader [B, Byte]) = new Reader [B, Short] {
    override def read (b : B) = read_short (b, r)
  }

  implicit def le_int_reader [B] (implicit r : Reader [B, Byte]) = new Reader [B, Int] {
    override def read (b : B) = read_int (b, r)
  }

  implicit def le_long_reader [B] (implicit r : Reader [B, Byte]) = new Reader [B, Long] {
    override def read (b : B) = read_long (b, r)
  }
}
