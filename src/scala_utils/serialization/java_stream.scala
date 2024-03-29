package scala_utils.serialization

import scala_utils.utils.Of

import java.io.{IOException, InputStream, OutputStream}

object java_stream {
  implicit val byte_writer: Writer [OutputStream, Byte] = (b: OutputStream, t: Byte) => b.write (t)

  implicit def byte_array_writer (implicit w: Writer [OutputStream, Int]): Writer [OutputStream, Array Of Byte] =
    (b: OutputStream, t: Array [Byte]) => w.write (b, t.length)

  implicit val byte_reader: Reader [InputStream, Byte] = (b: InputStream) => {
    val v = b.read ()

    if (v == -1)
      throw new IOException ()

    v.toByte
  }

  implicit def byte_array_reader (implicit r: Reader [InputStream, Int]): Reader [InputStream, Array Of Byte] =
    (b: InputStream) => {
      val l = r read b
      val a = new Array [Byte](l)

      if (b.read (a) != a.length)
        throw new IOException ()

      a
    }
}
