package scala_utils

import java.io.InputStream

package object serialization {
  def read [B, T] (b: B)(implicit r: Reader [B, T]): T = r.read (b)
  def write [B, T] (b: B, v: T)(implicit w: Writer [B, T]): Unit = w.write (b, v)

  def read [T] (b: InputStream)(implicit r: Reader [InputStream, T]): T = r.read (b)
}
