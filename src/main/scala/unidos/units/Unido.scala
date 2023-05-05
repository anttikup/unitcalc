package unidos.units

import unidos.units.{Dims, Unidos}


case class Unido(val multiplier: Double, val quantity: Quantity) {

  override def toString(): String =
    s"Unido($multiplier, '$quantity'; [name=${name}])"


  def *(scalar: Double): Unido =
    Unido(multiplier * scalar, quantity)

  def *(other: Unido): Unido = {
    val dimsThis = this.quantity
    val dimsOther = other.quantity

    val resultQuantity = dimsThis * dimsOther
    Unido(this.multiplier * other.multiplier, resultQuantity)
  }

  def /(scalar: Double): Unido =
    Unido(multiplier / scalar, quantity)

  def /(other: Unido): Unido = {
    val dimsThis = this.quantity
    val dimsOther = other.quantity

    val resultQuantity = dimsThis / dimsOther
    Unido(this.multiplier * other.multiplier, resultQuantity)
  }

  def name: Option[String] =
    Unidos.get(this) match {
      case Some(name) => Some(name)
      case None => Some(Unido.constructName(this))
    }

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

  def create(name: String, value: Unido): Unido =
    Unidos.create(name, value)

  def UNITLESS: Unido = Unidos.get("1").get

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
      throw new Error(s"Can't dismember elementary dims: $dims")
    }

    println(s"Construct name from: $dims")
    val unitsByDim = dims.zipWithIndex.map({
      case (dimensionExponent, i) =>
        if (dimensionExponent != 0)
          getExponentedUnitForDimension(i, dimensionExponent)
        else ""
    }).filter(value => value != "")

    println(s"  --> ${unitsByDim.mkString(" ")}")
    unitsByDim.mkString(" ")
  }

  def constructName(unit: Unido): String =
    constructName(unit.quantity.dims)

}
