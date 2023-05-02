package unidos.units

import scala.language.implicitConversions


class Unitful(val amount: Double, val unit: Unido) {

  def this(amount: Double) = this(amount, Unido.UNITLESS)

  override
  def toString(): String =
    s"Unitful($amount, $unit)"

  def +(other: Unitful) = {
    new Unitful(amount * unit.multiplier + other.amount * other.unit.multiplier, Unido.baseUnit(unit.dims))
  }

  def -(other: Unitful) =
    new Unitful(amount * unit.multiplier - other.amount * other.unit.multiplier, new Unido(1, unit.dims))

  def *(other: Unitful) =
    new Unitful(amount * other.amount, unit * other.unit)

  def *(scalar: Double) =
    new Unitful(amount * scalar, unit)

  def /(other: Unitful) =
    new Unitful(amount / other.amount, unit / other.unit)

  def ^(other: Unitful) = {
    if ( other.unit != Unido.UNITLESS ) {
      throw new Error("Not implemented")
    }
    val int = other.amount.toInt
    if ( int != other.amount ) {
      throw new Error("Not implemented")
    }
    new Unitful(Math.pow(amount, int), Unido.pow(unit, int))
  }

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
