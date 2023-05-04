package unidos.units

import scala.collection.mutable.HashMap


object Unidos {
  val byName = new HashMap[String, Unido]()
  val byValue = new HashMap[Unido, String]()

  create("1", Quantity.baseUnitOf("dimensionless"))

  def create(name: String, value: Unido): Unido = {
    if ( byName.contains(name) ) {
      val existingUnit = byName(name)
      if ( existingUnit != value ) {
        throw new Error(s"Unit already exists: $name")
      }
    } else {
      byName.put(name, value)
    }

    byValue.put(value, name)

    value
  }

  def get(name: String): Option[Unido] =
    byName.get(name)


  def get(value: Unido): Option[String] =
    byValue.get(value)

  def UNITLESS: Unido = get("1").get

  // def getUnitForDimension(dimension: Int, exponent: Int): String = {
  //   println(s"get unit for $dimension: $exponent")
  //   val quantity: String = Quantity.getDefaultQuantityForDimension(dimension, exponent)
  //   val unitValue = Quantity.baseUnitOf(quantity)
  //   byValue.get(unitValue) match {
  //     case Some(unit) => return s"${unit.name}${Util.getPower(exponent)}"
  //     case None => throw new Error(s"No default unit for dimension ${dimension}")
  //   }
  // }

  // def constructName(dims: Dims): String = {
  //   val unitsByDim = dims.zipWithIndex.map({
  //     case (dimensionValue, i) =>
  //       if (dimensionValue != 0)
  //         getUnitForDimension(i, dimensionValue)
  //       else ""
  //   }).filter(value => value != "")

  //   unitsByDim.mkString(" ")
  // }

  // def defaultName(quantity: String): String = {
  //   defaultUnits.get(quantity) match {
  //     case Some(unit) => unit.name
  //     case None => {
  //       Quantity.get(quantity) match {
  //         case Some(dims) => constructName(dims)
  //         case None => throw new Error(s"Couldn't get dimensions for quantity $quantity")
  //       }
  //     }
  //   }
  // }

  def getDefaultUnitForQuantity(quantity: String): Unido =
    Quantity.baseUnitOf(quantity)


  // def defaultName(unit: Unido): String = {
  //   s"${unit.multiplier}·${defaultName(unit.quantity)}"
  // }

  // def nameOrDefaultName(unit: Unido): String = {
  //   unit.name match {
  //     case "" => defaultName(unit)
  //     case _ => unit.name
  //   }
  // }
}
