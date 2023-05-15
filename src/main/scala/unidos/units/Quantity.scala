package unidos.units

import scala.collection.mutable.HashMap


case class Quantity(val name: String, dims: Dims) {
  override def toString: String = {
    return s"Quantity(\"$name\", $dims)"
  }

  def *(other: Quantity): Quantity = {
    if ( this.name == "dimensionless" ) { // TODO: don't check by name
      return other
    } else if ( other.name == "dimensionless" ) {
      return this
    }
    val newDims = this.dims * other.dims
    Quantity.byDims.get(newDims) match {
      case Some(quantity) => quantity
      case None => new ImplicitQuantity(Quantity.constructName(newDims), newDims)
    }
  }

  def /(other: Quantity): Quantity = {
    val newDims = this.dims / other.dims
    Quantity.byDims.get(newDims) match {
      case Some(quantity) => quantity
      case None => new ImplicitQuantity(Quantity.constructName(newDims), newDims)
    }
  }

  def isDimensionless: Boolean =
    dims.isDimensionless

}
class ImplicitQuantity(override val name: String, dims: Dims) extends Quantity(name, dims)
class NamedQuantity(override val name: String, dims: Dims) extends Quantity(name, dims)


object Quantity {
  val byName = new HashMap[String, NamedQuantity]()
  val byDims = new HashMap[Dims, NamedQuantity]()

  def create(name: String, dims: Dims): NamedQuantity = {
    val quantity = new NamedQuantity(name, dims)
    byName.put(name, quantity)
    // Keep the first name as the default name
    if ( !byDims.contains(dims) ) {
      byDims.put(dims, quantity)
    }
    quantity
  }

  def create(name: String, quantity: Quantity): NamedQuantity =
    create(name, quantity.dims)

  def createBaseQuantities(names: Array[String]): Array[Quantity] = {
    Dims.numberOfDims = names.length - 1

    var output = Array[Quantity]()
    for ( case (name, index) <- names.zipWithIndex ) {
      if ( index == 0 ) {
        output = output :+ create(names(0), Dims(new Array[Int](Dims.numberOfDims):_*))
      } else {
        output = output :+ create(
          name,
          Dims.makeOneDimensionDims(index - 1)
        )
      }
    }

    output
  }


  def get(dims: Dims): Quantity = {
    byDims.get(dims) match {
      case Some(quantity) => quantity
      case None => new ImplicitQuantity(constructName(dims), dims)
    }
  }

  def get(name: String): Option[Quantity] =
    byName.get(name)


  def clear: Unit = {
    byName.clear
    byDims.clear
  }

  def pow(quantity: Quantity, exp: Int): Quantity = {
    val newDims = Dims.pow(quantity.dims, exp)
    Quantity.byDims.get(newDims) match {
      case Some(quantity) => quantity
      case None => new ImplicitQuantity(Quantity.constructName(newDims), newDims)
    }
  }

  def root(quantity: Quantity, exp: Int): Quantity = {
    val newDims = Dims.root(quantity.dims, exp)
    Quantity.byDims.get(newDims) match {
      case Some(quantity) => quantity
      case None => new ImplicitQuantity(Quantity.constructName(newDims), newDims)
    }
  }


  def getDefaultQuantityForDimension(dimension: Int, exponent: Int): Quantity = {
    val dims = Dims.makeOneDimensionDims(dimension, exponent)
    Quantity.get(dims)
  }

  def baseUnitOf(quantity: String): Unido = {
    val q = Quantity.get(quantity).get
    Unido(1, q)
  }

  def getExponentedQuantityForDimension(dimension: Int, exponent: Int): String = {
    val quantity = Quantity.getDefaultQuantityForDimension(dimension, 1)
    quantity.name + Util.getPower(exponent)
  }


  def constructName(dims: Dims): String = {
    val quantitiesByDim = dims.zipWithIndex.map({
      case (dimensionExponent, i) =>
        if (dimensionExponent != 0)
          getExponentedQuantityForDimension(i, dimensionExponent)
        else ""
    }).filter(value => value != "")

    quantitiesByDim.mkString(" ")
  }


}
