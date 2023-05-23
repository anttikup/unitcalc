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
        if ( existingUnit.quantity == value.quantity ) {
          throw new Error(s"Unit ”$name” already exists with a different multiplier: "
                          + s"(was: ${existingUnit.multiplier}; now: ${value.multiplier})")
        } else if ( existingUnit.multiplier == value.multiplier ) {
          throw new Error(s"Unit ”$name” already exists with a different quantity: "
                          + s"(was: ${existingUnit.quantity.name}; now: ${value.quantity.name})")
        } else {
          throw new Error(s"Unit ”$name” already exists with a different definition: "
                          + s"(was: quantity: ${existingUnit.quantity.name}, value: ${existingUnit.multiplier}; "
                          + s"now: quantity: ${value.quantity.name}, value: ${value.multiplier})")
        }
      }
    } else {
      byName.put(name, value)
    }


    if ( !byValue.contains(value) ) {
      byValue.put(value, name)
    }

    value
  }

  def create(compoundName: CompoundName, value: Unido): Unido =
    create(compoundName.toString, value)

  def get(name: String): Option[Unido] =
    byName.get(name)

  def get(value: Unido): Option[String] =
    byValue.get(value)

  def clear: Unit = {
    byName.clear
    byValue.clear
    create("1", Quantity.baseUnitOf("dimensionless"))
  }

  def UNITLESS: Unido = get("1").get

  def getDefaultUnitForQuantity(quantity: String): Unido =
    Quantity.baseUnitOf(quantity)

}
