package unidos.units

import scala.collection.mutable.HashMap


class Unido(val name: String, val multiplier: Double, val dims: Dims) {

  def this(multiplier: Double, dims: Dims) =
    this(Unido.defaultName(dims), multiplier, dims)

  override def toString(): String =
    s"Unido('$name', $multiplier, $dims)"

  def +(other: Unido) = {
    if ( dims != other.dims ) {
      throw new Error(s"Incompatible dimensions: ”${dims.name.get}” and ”${other.dims.name.get}”")
    }

    if ( multiplier == other.multiplier ) {
      new Unido(name, multiplier + other.multiplier, dims)
    } else {
      new Unido("", multiplier + other.multiplier, dims)
    }
  }

  def -(other: Unido) = {
    if ( dims != other.dims ) {
      throw new Error("Incompatible dimensions")
    }

    new Unido(name, multiplier - other.multiplier, dims)
  }

  def *(other: Unido): Unido = {
    if ( this == other ) {
      new Unido("", multiplier, dims * other.dims)
    } else if ( other.dims == Dims.get("Dimensionless").get ) {
      if ( other.name == "1" ) {
        new Unido(name, multiplier * other.multiplier, dims * other.dims)
      } else if ( this.name == "1" ) {
        new Unido(other.name, multiplier * other.multiplier, dims * other.dims)
      } else {
        new Unido(name, multiplier * other.multiplier, dims * other.dims)
      }
    } else if ( dims == Dims.get("Dimensionless").get ) {
      new Unido(other.name, multiplier * other.multiplier, dims * other.dims)
    } else if ( dims == other.dims ) {
      new Unido(multiplier * other.multiplier, dims * other.dims)
    } else {
      new Unido(multiplier * other.multiplier, dims * other.dims)
    }
  }

  def *(scalar: Double): Unido = new Unido("", multiplier * scalar, dims)

  def /(other: Unido): Unido = {
    if ( other.dims == Dims.get("Dimensionless").get ) {
      new Unido(name, multiplier / other.multiplier, dims / other.dims)
    } else {
      new Unido(multiplier / other.multiplier, dims / other.dims)
    }
  }

  def /(scalar: Double): Unido = new Unido(multiplier / scalar, dims)

  def /:(other: Double): Unido = {
    println(s"other: $other: ${multiplier / other}")
    new Unido(multiplier / other, Dims(0, 0, 0, 0, 0, 0, 0) / dims)
  }

  def quantity: Option[String] =
    dims.name

  def nameOrDefaultName: String =
    Unido.nameOrDefaultName(this)

}


object Unido {
  val byName = new HashMap[String, Unido]()
  val defaultUnits = new HashMap[Dims, Unido]()

  put("1", 1, Dims.get("Dimensionless").get)

  def apply(name: String = "") = {
    get(name) match {
      case Some(unit) => unit
      case None => {
        throw new Error(s"No such unit $name")
      }
    }
  }

  def put(name: String, multiplier: Double, dims: Dims): Unido = {
    val unit = new Unido(name, multiplier, dims)
    byName.put(name, unit)
    println(s"Add unit $name")
    if ( multiplier == 1 && !defaultUnits.contains(dims) ) {
      println(s"default $dims : $unit")
      defaultUnits.put(dims, unit)
    }
    unit
  }

  def put(name: String, unit: Unido): Unido = {
    put(name, unit.multiplier, unit.dims)
  }

  def get(name: String): Option[Unido] =
    byName.get(name)

  def UNITLESS: Unido = get("1").get

  def baseUnit(quantity: Dims): Unido =
    new Unido(defaultName(quantity), 1, quantity)

  def pow(unit: Unido, exp: Int) =
    new Unido(math.pow(unit.multiplier, exp), Dims.pow(unit.dims, exp))

  def root(unit: Unido, exp: Int) =
    new Unido(math.pow(unit.multiplier, 1/exp), Dims.root(unit.dims, exp))

  def getUnitForDimension(dimension: Int, exponent: Int): String = {
    println(s"get unit for $dimension: $exponent")
    val part = Dims.makeOneDimensionDims(dimension, 1)
    defaultUnits.get(part) match {
      case Some(unit) => return s"${unit.name}${Util.getPower(exponent)}"
      case None => throw new Error(s"No default unit for dimension ${dimension}")
    }
  }

  def constructName(dims: Dims): String = {
    val unitsByDim = dims.zipWithIndex.map({
      case (dimensionValue, i) =>
        if (dimensionValue != 0)
          getUnitForDimension(i, dimensionValue)
        else ""
    }).filter(value => value != "")

    unitsByDim.mkString(" ")
  }

  def defaultName(dims: Dims): String = {
    defaultUnits.get(dims) match {
      case Some(unit) => unit.name
      case None => constructName(dims)
    }
  }


  def defaultName(unit: Unido): String = {
    s"${unit.multiplier}·${defaultName(unit.dims)}"
  }

  def nameOrDefaultName(unit: Unido): String = {
    unit.name match {
      case "" => defaultName(unit)
      case _ => unit.name
    }
  }
}
