package unidos.units

import scala.language.implicitConversions


/**
 * A value in some unit.
 */
class Unitful(val amount: Double, val unit: Unido) {
  def this(amount: Double) =
    this(amount, Unido.UNITLESS)

  override
  def toString(): String =
    s"Unitful($amount, $unit)"

  def +(other: Unitful): Unitful = {
    if ( this.unit.quantity != other.unit.quantity ) {
      throw new Error(s"Incompatible units: ${this.unit} and ${other.unit}")
    }

    val resultUnit = Quantity.baseUnitOf(this.unit.quantity.name)
    Unitful(this.amount * this.unit.multiplier + other.amount * other.unit.multiplier, resultUnit)
  }

  def -(other: Unitful): Unitful = {
    if ( this.unit.quantity != other.unit.quantity ) {
      throw new Error(s"Incompatible units: ${this.unit} and ${other.unit}")
    }

    val resultUnit = Quantity.baseUnitOf(this.unit.quantity.name)
    Unitful(this.amount * this.unit.multiplier - other.amount * other.unit.multiplier, resultUnit)
  }

  def *(scalar: Double): Unitful =
    Unitful(amount * scalar, unit)

  def *(other: Unitful): Unitful =
    Unitful(amount * other.amount, unit * other.unit)

  def /(scalar: Double): Unitful =
    Unitful(amount / scalar, unit)

  def /(other: Unitful): Unitful =
    Unitful(amount / other.amount, unit / other.unit)

  def ===(other: Unitful) =
    this.unit.quantity == other.unit.quantity &&
      (Util.almostEquals(this.numericalValue, other.numericalValue))

  def quantity = unit.quantity

  def numericalValue: Double =
    this.amount * this.unit.multiplier

  def normalized: Unitful = {
    val unit = Unido(1, this.quantity)
    new Unitful(this.amount * this.unit.multiplier, unit)
  }

  def isDimensionless: Boolean =
    unit.isDimensionless



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
    val newUnitful = new Unitful(amount, unit)
    // If the unit of the new value doesn't have a name, normalize it to base unit.
    // Eg. (assuming dm² doesn't exist) 4 dm² = 4 0.01m² -> 0.04 m²
    newUnitful
    // Unidos.get(unit) match {
    //   case Some(name) => newUnitful
    //   case None => newUnitful.normalized
    // }



  def apply(amount: Double) =
    new Unitful(amount)


  def pow(unitfulValue: Unitful, exp: Int): Unitful = {
    val newUnit = Unido.pow(unitfulValue.unit, exp)
    Unitful(Math.pow(unitfulValue.amount, exp), newUnit)
  }

  def pow(unitfulValue: Unitful, exp: Double): Unitful = {
    if ( exp.toInt == exp ) {
      return pow(unitfulValue, exp.toInt)
    }

    if ( !unitfulValue.isDimensionless ) {
      throw new Error("Can only raise dimensionless values to an arbitrary power")
    }
    Unitful(Math.pow(unitfulValue.amount, exp), unitfulValue.unit)
  }

  def pow(unitfulValue: Unitful, exp: Unitful): Unitful = {
    if ( !exp.isDimensionless ) {
      throw new Error("Can only raise value to dimensionless power")
    }
    pow(unitfulValue, exp.numericalValue)
  }

  def root(unitfulValue: Unitful, exp: Int) =
    Unitful(Math.pow(unitfulValue.amount, 1/exp.toDouble), Unido.root(unitfulValue.unit, exp))

  def root(unitfulValue: Unitful, exp: Double): Unitful = {
    if ( exp.toInt == exp ) {
      return root(unitfulValue, exp.toInt)
    }

    if ( !unitfulValue.isDimensionless ) {
      throw new Error("Can only take arbitrary root of dimensionless values")
    }
    Unitful(Math.pow(unitfulValue.amount, 1/exp), unitfulValue.unit)
  }

  def root(unitfulValue: Unitful, exp: Unitful): Unitful = {
    if ( !exp.isDimensionless ) {
      throw new Error("Can only raise value to dimensionless power")
    }
    root(unitfulValue, exp.numericalValue)
  }

}
