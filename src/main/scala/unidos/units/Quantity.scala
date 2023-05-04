package unidos.units

import scala.collection.mutable.HashMap


// class Quantity(name: String, args: Int*) extends Dims(args*) {

// }


object Quantity {
  val byName = new HashMap[String, Dims]()
  val byDims = new HashMap[Dims, String]()

  create("dimensionless", Dims(new Array[Int](Axis.values.size):_*))
  for (d <- Axis.values) {
    create(
      d.toString,
      Dims.makeOneDimensionDims(d.id)
    )
  }

  def create(name: String, dims: Dims): Dims = {
    byName.put(name, dims)
    // Keep the first name as the default name
    if ( !byDims.contains(dims) ) {
      println(s"Added dims: $name, $dims")
      byDims.put(dims, name)
    }
    dims
  }

  def get(dims: Dims): String = {
    byDims.get(dims) match {
      case Some(name) => name
      case None => constructName(dims)
    }
  }

  def get(name: String): Option[Dims] =
    byName.get(name)

  def getDefaultQuantityForDimension(dimension: Int, exponent: Int): String = {
    val dims = Dims.makeOneDimensionDims(dimension, exponent)
    Quantity.get(dims)
  }

  def baseUnitOf(quantity: String): Unido =
    Unido(1, quantity)


  def getExponentedQuantityForDimension(dimension: Int, exponent: Int): String = {
    println(s"get quantity for $dimension: $exponent")
    val quantity: String = Quantity.getDefaultQuantityForDimension(dimension, 1)
    quantity + Util.getPower(exponent)
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
