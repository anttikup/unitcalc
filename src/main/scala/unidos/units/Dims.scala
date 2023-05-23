package unidos.units

object Axis extends Enumeration {
  type Axis = Value
  val time, length, mass, `electric current`, temperature,
      `amount of substance`, `luminous intensity` = Value
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

  def isElementary: Boolean =
    args.sum == 1 && args.count(_ == 0) == Dims.numberOfDims - 1

  def isDimensionless: Boolean =
    args.forall(_ == 0)

}

object Dims {
  var numberOfDims = 0
  def makeOneDimensionDims(dimension: Int, value: Int = 1): Dims = {
    val arr = new Array[Int](numberOfDims)
    arr(dimension) = value
    Dims(arr:_*)
  }

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
