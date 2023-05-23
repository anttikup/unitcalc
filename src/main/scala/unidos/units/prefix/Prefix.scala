package unidos.units.prefix

import unidos.units.{Unido, Unidos, Quantity}


class Prefix {
  val byText = Map[String, Double]()

  def createUnits(baseName: String, quantity: Quantity, unprefixedValue: Double = 1) = {

    for ( case (prefix, value) <- byText ) {
      val unit = new Unido(unprefixedValue * value, quantity)
      Unidos.create(s"$prefix$baseName", unit)
    }
  }

  def value(text: String): Double = {
    byText(text)
  }

}
