package scala_utils.serialization

import java.io.{IOException, InputStream, OutputStream}

import scala_utils.utils.Of

object java_stream {
  implicit val byte_writer = new Writer [OutputStream, Byte] {
    override def write (b : OutputStream, t : Byte) = b.write (t)
  }

  implicit def byte_array_writer (implicit w : Writer [OutputStream, Int]) = new Writer [OutputStream, Array Of Byte] {
    override def write (b : OutputStream, t : Array [Byte]) = w.write (b, t.length)
  }

  implicit val byte_reader = new Reader [InputStream, Byte] {
    override def read (b : InputStream) = {
      val v = b.read ()

      if (v == -1)
        throw new IOException ()

      v.toByte
    }
  }

  implicit def byte_array_reader (implicit r : Reader [InputStream, Int]) = new Reader [InputStream, Array [Byte]] {
    override def read (b : InputStream) = {
      val l = r read b
      val a = new Array [Byte](l)

      if (b.read (a) != a.length)
        throw new IOException ()

      a
    }
  }
}
