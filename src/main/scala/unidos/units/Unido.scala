package unidos.units

import unidos.units.{Dims, Unidos}
import unidos.units.prefix.Prefix




case class Unido(val multiplier: Double, val quantity: Quantity) {

  override def toString(): String =
    name match {
      case Some(name) => s"Unido($multiplier, $quantity; [name=\"${name}\", prefix=TODO])"
      case None => s"Unido($multiplier, $quantity; [name=?, prefix=TODO])"
    }


  def *(scalar: Double): Unido =
    Unido(multiplier * scalar, quantity)

  def *(other: Unido): Unido = {
    val dimsThis = this.quantity
    val dimsOther = other.quantity

    val resultQuantity = dimsThis * dimsOther
    val resultUnit = Unido(this.multiplier * other.multiplier, resultQuantity)

    Unidos.get(resultUnit) match {
      case Some(name) => resultUnit
      case None => {
        new CompoundUnido(CompoundName(this.name.get -> 1, other.name.get -> 1), resultUnit)
      }
    }

  }

  def /(scalar: Double): Unido =
    Unido(multiplier / scalar, quantity)

  def /(other: Unido): Unido = {
    val dimsThis = this.quantity
    val dimsOther = other.quantity

    val resultQuantity = dimsThis / dimsOther
    val resultUnit = Unido(this.multiplier / other.multiplier, resultQuantity)

    Unidos.get(resultUnit) match {
      case Some(name) => resultUnit
      case None => {
        new CompoundUnido(CompoundName(this.name.get -> 1, other.name.get -> -1), resultUnit)
      }
    }

  }

  def ===(other: Unido): Boolean =
    this.quantity == other.quantity && Util.almostEquals(this.multiplier, other.multiplier)

  def name: Option[String] =
    Unidos.get(this) match {
      case Some(name) => Some(name)
      case None => Some(Unido.constructName(this))
    }


  def isDimensionless =
    quantity.isDimensionless

}


object Unido {
  def apply(name: String) = {
    Unidos.get(name) match {
      case Some(unit) => unit
      case None => {
        throw new Error(s"No such unit $name")
      }
    }
  }

  def create(name: String, multiplier: Double, quantity: Quantity): Unido = {
    val value = new Unido(multiplier, quantity)
    Unidos.create(name, value)
  }

  def create(name: CompoundName, multiplier: Double, quantity: Quantity): Unido = {
    val value = new Unido(multiplier, quantity)
    Unidos.create(name, value)
  }

  def create(name: String, value: Unido): Unido =
    Unidos.create(name, value)

  def create(name: CompoundName, value: Unido): Unido =
    Unidos.create(name, value)

  def UNITLESS: Unido = Unidos.get("1").get

  def pow(unit: Unido, exp: Int): Unido = {
    val newQuantity = Quantity.pow(unit.quantity, exp)
    new Unido(Math.pow(unit.multiplier, exp), newQuantity)
  }

  def root(unit: Unido, exp: Int): Unido = {
    val newQuantity = Quantity.root(unit.quantity, exp)
    new Unido(Math.pow(unit.multiplier, 1/exp), newQuantity)
  }

  def getDefaultUnitForDimension(dimension: Int, exponent: Int): Unido = {
    val quantity = Quantity.getDefaultQuantityForDimension(dimension, exponent)

    Unido(1, quantity)
  }

  def getExponentedUnitForDimension(dimension: Int, exponent: Int): String = {
    val unit = Unido.getDefaultUnitForDimension(dimension, 1)
    Unidos.get(unit) match {
      case Some(name) => {
        name + Util.getPower(exponent)
      }
      case None => throw new Error(s"No basic unit defined for dimension $dimension")
    }
  }

  def constructName(dims: Dims): String = {
    if ( dims.isElementary ) {
      //throw new Error(s"Can't dismember elementary dims: $dims")
      return "?"
    }

    val unitsByDim = dims.zipWithIndex.map({
      case (dimensionExponent, i) =>
        if (dimensionExponent != 0)
          getExponentedUnitForDimension(i, dimensionExponent)
        else ""
    }).filter(value => value != "")

    unitsByDim.mkString(" ")
  }

  def constructName(unit: Unido): String =
    constructName(unit.quantity.dims)

}

class CompoundUnido(val _name: CompoundName, val unit: Unido) extends Unido(unit.multiplier, unit.quantity) {
  override def name: Option[String] = Some(_name.toString)
}
