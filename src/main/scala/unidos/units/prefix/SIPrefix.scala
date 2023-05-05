package unidos.units.prefix

import scala.collection.mutable.HashMap

import unidos.units.{Quantity, Unido, Unidos}


class SIPrefix(override val text: String = "") extends Prefix(text) {

  override def value: Double = {
    SIPrefix.value(text)
  }

}


object SIPrefix {
  val byText = HashMap[String, Double](
    "mega" -> 1e6,
    "kilo" -> 1e3,
    "" -> 1,
    "milli" -> 1e-3,
    "micro" -> 1e-6,
  )

  def value(text: String): Double = {
    byText(text)
  }

  def createUnits(baseName: String, quantity: Quantity, unprefixedValue: Double = 1) = {

    for ( case (prefix, value) <- byText ) {
      val unit = new Unido(unprefixedValue * value, quantity, SIPrefix(prefix))
      Unidos.create(s"$prefix$baseName", unit)
      println(s"Created unit: $unit")
    }
  }

}
