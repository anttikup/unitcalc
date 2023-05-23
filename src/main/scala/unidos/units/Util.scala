package unidos.units

object Util {
  val EPSILON = 0.0000000001
  val pows = Array("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "⁻")

  def getPower(num: Int): String = {
    val numStr = s"$num"
    numStr.map(digit => if (digit == '-') pows(10) else pows(digit - '0')).mkString("")
  }

  def almostEquals(a: Double, b: Double, precision: Double = EPSILON) =
    (a - b).abs < Math.pow(10, -precision)


}
