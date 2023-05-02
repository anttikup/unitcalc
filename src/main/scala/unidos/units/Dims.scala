package unidos.units

import scala.collection.mutable.HashMap


object Axis extends Enumeration {
  type Axis = Value
  val Time, Length, Mass, ElectricCurrent, Temperature, AmountOfSubstance, LuminousIntensity = Value
}



case class Dims(args: Int*) {
  val dims = args.toArray

  override def toString: String = {
    return s"Dims(${dims.mkString(", ")})"
  }

  def zipWithIndex = dims.zipWithIndex

  def *(other: Dims): Dims = {
    return new Dims(dims.zip(other.dims).map({ case (a, b) => a + b }):_*)
  }

  def /(other: Dims): Dims = {
    return new Dims(dims.zip(other.dims).map({ case (a, b) => a - b }):_*)
  }

  def name: Option[String] =
    Dims.get(this)

  override def hashCode(): Int =
    java.util.Arrays.hashCode(dims)

}

object Dims {
  val byName = new HashMap[String, Dims]()
  val byDims = new HashMap[Dims, String]()


  name(Dims(new Array[Int](Axis.values.size):_*), "Dimensionless")
  for (d <- Axis.values) {
    name(makeOneDimensionDims(d.id), d.toString)
  }

  def makeOneDimensionDims(dimension: Int, value: Int = 1): Dims = {
    val arr = new Array[Int](Axis.values.size)
    arr(dimension) = value
    Dims(arr:_*)
  }

  def name(dims: Dims, name: String) = {
    byDims.put(dims, name)
    byName.put(name, dims)
    dims
  }

  def get(dims: Dims): Option[String] =
    byDims.get(dims)

  def get(name: String): Option[Dims] =
    byName.get(name)

  def pow(dims: Dims, pow: Int): Dims = {
    return new Dims(dims.dims.map({ case (dim) => dim * pow }):_*)
  }

  def root(dims: Dims, r: Int): Dims = {
    val tmp = new Dims(dims.dims.map({ case (dim) => dim / r }):_*)
    if ( pow(tmp, r) != dims ) {
      throw new Error(s"Can't take $r-root of $this")
    }
    tmp
  }

}
