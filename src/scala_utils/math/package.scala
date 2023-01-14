package scala_utils

package object math {
  val log2 = scala.math.log (2)
  val log10_2 = scala.math.log10 (2)

  type Point_2I = Point_2 [Int]
  type Point_2F = Point_2 [Float]
  type Point_2D = Point_2 [Double]

  type Range_int = Range [Int]
  type Range_float = Range [Float]
  type Range_double = Range [Double]

  def db_to_amp (db: Double): Double = scala.math.sqrt (scala.math.pow (10, db / 10))
  def interpolate (f: Double, a: Double, b: Double): Double = lerp (f, a, b)
  def lerp (f: Double, a: Double, b: Double): Double = (1 - f) * a + f * b
  def sinc (f: Double): Double = if (f == 0) 1.0 else Math.sin (f) / f
  def sinc_normalized (f: Double): Double = sinc (Math.PI * f)
  val sqrt_2_recip = Math.sqrt (2).recip

  /**
   * sinc_normalized(sinc_normalized_cutoff) = 1 / sqrt(2)
   *
   * Estimated using iteration
   */
  val sinc_normalized_cutoff = 0.44294647069182247
  def sinc_normalized_inverse (y: Double): Double = inverse_monotonic (0.5, -0.25, y, sinc_normalized)

  @scala.annotation.tailrec
  def inverse_monotonic (x: Double, dx: Double, target: Double, f: Double => Double,
                         max_error: Double = 0.0000000001): Double = {
    val y = f (x)

    if (Math.abs (y - target) <= max_error)
      x
    else
      inverse_monotonic (x + (if (y < target) dx else -dx), dx * 0.5, target, f, max_error)
  }

  implicit class Math_double_ops (val value: Double) extends AnyVal {
    def recip: Double = 1.0 / value
  }
}
