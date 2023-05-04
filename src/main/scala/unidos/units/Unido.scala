package unidos.units

import unidos.units.{Dims, Unidos}


case class Unido(val multiplier: Double, val quantity: String) {

  Quantity.get(quantity) match {
    case Some(_) => ;
    case None => ;//throw new Error(s"No such quantity: $quantity")
  }

  override def toString(): String =
    s"Unido($multiplier, '$quantity'; [name=${name}])"


  def *(scalar: Double): Unido =
    Unido(multiplier * scalar, quantity)

  def *(other: Unido): Unido = {
    val dimsThis = Quantity.get(this.quantity)
    val dimsOther = Quantity.get(other.quantity)
    println(s"dims: ${this.quantity}: $dimsThis; ${other.quantity}: $dimsOther")

    val resultDims = dimsThis.get * dimsOther.get
    val quantity = Quantity.get(resultDims)
    Unido(this.multiplier * other.multiplier, quantity)
  }

  def /(scalar: Double): Unido =
    Unido(multiplier / scalar, quantity)

  def /(other: Unido): Unido = {
    val dimsThis = Quantity.get(this.quantity)
    val dimsOther = Quantity.get(other.quantity)
    println(s"dims: ${this.quantity}: $dimsThis; ${other.quantity}: $dimsOther")

    val resultDims = dimsThis.get / dimsOther.get
    val quantity = Quantity.get(resultDims)
    Unido(this.multiplier * other.multiplier, quantity)
  }

  def name: Option[String] =
    Unidos.get(this) match {
      case Some(name) => Some(name)
      case None => Some(Unido.constructName(this.quantity))
    }

}


object Unido {
  def apply(name: String = "") = {
    Unidos.get(name) match {
      case Some(unit) => unit
      case None => {
        throw new Error(s"No such unit $name")
      }
    }
  }

  def create(name: String, multiplier: Double, quantity: String): Unido = {
    val value = new Unido(multiplier, quantity)
    Unidos.create(name, value)
  }

  def create(name: String, value: Unido): Unido =
    Unidos.create(name, value)

  def UNITLESS: Unido = Unidos.get("1").get

  def getDefaultUnitForDimension(dimension: Int, exponent: Int): Unido = {
    val quantity = Quantity.getDefaultQuantityForDimension(dimension, exponent)

    Quantity.baseUnitOf(quantity)
  }

  def getExponentedUnitForDimension(dimension: Int, exponent: Int): String = {
    println(s"get unit for $dimension: $exponent")
    val unit = Unido.getDefaultUnitForDimension(dimension, 1)
    unit.name.get + Util.getPower(exponent)
  }

  def constructName(dims: Dims): String = {
    val unitsByDim = dims.zipWithIndex.map({
      case (dimensionExponent, i) =>
        if (dimensionExponent != 0)
          getExponentedUnitForDimension(i, dimensionExponent)
        else ""
    }).filter(value => value != "")

    unitsByDim.mkString(" ")
  }

  def constructName(quantity: String): String =
    Quantity.get(quantity) match {
      case Some(dims) => constructName(dims)
      case None => throw new Error(s"no such quantity: $quantity")
    }

}
