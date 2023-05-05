package unidos.units

import scala.language.implicitConversions


class Unitful(val amount: Double, val unit: Unido) {

  def this(amount: Double) = this(amount, Unido.UNITLESS)

  override
  def toString(): String =
    s"Unitful($amount, $unit)"

  def +(other: Unitful) = {
    if ( this.unit.quantity != other.unit.quantity ) {
      throw new Error(s"Incompatible units: ${this.unit} and ${other.unit}")
    }

    val resultUnit = Quantity.baseUnitOf(this.unit.quantity.name)
    new Unitful(this.amount * this.unit.multiplier + other.amount * other.unit.multiplier, resultUnit)
  }

  def -(other: Unitful) = {
    if ( this.unit.quantity != other.unit.quantity ) {
      throw new Error(s"Incompatible units: ${this.unit} and ${other.unit}")
    }

    val resultUnit = Quantity.baseUnitOf(this.unit.quantity.name)
    new Unitful(this.amount * this.unit.multiplier - other.amount * other.unit.multiplier, resultUnit)
  }

  def *(scalar: Double) =
    new Unitful(amount * scalar, unit)

  def *(other: Unitful) =
    new Unitful(amount * other.amount, unit * other.unit)

  def /(scalar: Double) =
    new Unitful(amount / scalar, unit)

  def /(other: Unitful) =
    new Unitful(amount / other.amount, unit / other.unit)

  def quantity = unit.quantity

  // TODO: these don't work
  implicit def doubleToUnitful(amount: Double): Unitful =
    new Unitful(amount)

  implicit
  def intToUnitful(amount: Int): Unitful =
    new Unitful(amount)

  implicit
  def unidoToUnitful(unit: Unido): Unitful =
    new Unitful(1, unit)
}

object Unitful {
  def apply(amount: Double, unit: Unido) =
    new Unitful(amount, unit)

  def apply(amount: Double) =
    new Unitful(amount)

}
