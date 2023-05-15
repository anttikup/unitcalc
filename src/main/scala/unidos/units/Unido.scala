package unidos.units

import unidos.units.{Dims, Unidos}
import unidos.units.prefix.Prefix




case class Unido(val multiplier: Double, val quantity: Quantity) {

  override def toString(): String =
    s"Unido($multiplier, $quantity; [name=\"${name}\", prefix=TODO])"


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
        new CompoundUnido(this.name * other.name, resultUnit)
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
      case None =>
        CompoundUnido(this.name / other.name, resultUnit)
    }

  }

  def ===(other: Unido): Boolean =
    this.quantity == other.quantity && Util.almostEquals(this.multiplier, other.multiplier)

  def name: CompoundName = {
    Unidos.get(this) match {
      case Some(name) => CompoundName.fromString(name)
      case None => Unido.constructName(this)
    }
  }

  def isDimensionless =
    quantity.isDimensionless

  def value = this
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
    Unidos.create(name, value.value)
  }

  def create(name: CompoundName, multiplier: Double, quantity: Quantity): Unido = {
    val value = new Unido(multiplier, quantity)
    Unidos.create(name, value.value)
  }

  def create(name: String, value: Unido): Unido =
    Unidos.create(name, value.value)

  def create(name: CompoundName, value: Unido): Unido =
    Unidos.create(name, value.value)

  def UNITLESS: Unido =
    Unidos.get("1").get

  def pow(unit: Unido, exp: Int): Unido = {
    val newQuantity = Quantity.pow(unit.quantity, exp)
    val resultUnit = new Unido(Math.pow(unit.multiplier, exp), newQuantity)

    Unidos.get(resultUnit) match {
      case Some(name) => resultUnit
      case None => {
        val name = CompoundName.pow(unit.name, exp)
        new CompoundUnido(name, resultUnit)
      }
    }
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

  def constructName(dims: Dims): CompoundName = {
    if ( dims.isElementary ) {
      //throw new Error(s"Can't dismember elementary dims: $dims")
      return CompoundName("?" -> 1)
    }

    val unitsByDim = dims.zipWithIndex.map({
      case (dimensionExponent, i) =>
        (getDefaultUnitForDimension(i, 1).name.toString, dimensionExponent)
    }).filter(tuple => tuple._1 != "1" && tuple._2 != 0)

    CompoundName(unitsByDim:_*)
  }

  def constructName(unit: Unido): CompoundName =
    constructName(unit.quantity.dims)

}

class CompoundUnido(override val name: CompoundName, val unit: Unido) extends Unido(unit.multiplier, unit.quantity) {
  override def value = unit
}
