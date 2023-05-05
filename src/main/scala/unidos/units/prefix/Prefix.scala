package unidos.units.prefix

import unidos.units.{Unido, Unidos}


class Prefix(val text: String = "") {

  override def toString: String =
    s"""Prefix("$text")"""

  def value: Double =
    text match {
      case "" => 1
    }


  def *(otherUnit: Unido): Unido = {
    if ( otherUnit.prefix.text != "" ) {
      throw new Error(s"Can't prefix prefixed unit: $otherUnit")
    }

    val newUnit = new Unido(this.value * otherUnit.multiplier, otherUnit.quantity, this)

    Unidos.create(this.text + otherUnit.name, newUnit)
  }
}
