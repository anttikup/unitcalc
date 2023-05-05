package unidos

import unidos.units.{Dims, Quantity, Unido, Unidos}
import unidos.units.prefix.{SIPrefix}



class PrefixTest extends munit.FunSuite {
  def createBasicDims: Array[Quantity] =
    Quantity.createBaseQuantities(
      Array(
        "dimensionless",
        "time",
        "length",
        "mass",
        "electric current",
        "temperature",
        "amount of substance",
        "luminous intensity"
      )
    )



  test("can create prefixed units") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    SIPrefix.createUnits("metre", length)

    val km = Unido("kilometre")
    val m = Unido("metre")

    assert(km.multiplier == 1000)
    assert(km.quantity == Quantity.get("length").get)

    assert(m.multiplier == 1)
    assert(m.quantity == Quantity.get("length").get)

    assert(Quantity.baseUnitOf("length") == m)
  }

  test("can create prefixed units with custom unprefixed value") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    SIPrefix.createUnits("gram", mass, 1e-3)

    val kg = Unido("kilogram")
    val g = Unido("gram")

    assert(kg.multiplier == 1)
    assert(kg.quantity == Quantity.get("mass").get)

    assert(g.multiplier == 1e-3)
    assert(g.quantity == Quantity.get("mass").get)

    assert(Quantity.baseUnitOf("mass") == kg)
  }

}
