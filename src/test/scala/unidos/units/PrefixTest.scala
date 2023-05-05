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


  // test("can create a unit by multiplying other unit by prefix") {
  //   val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

  //   val m = Unido.create("metre", 1, length)
  //   val km = new SIPrefix("kilo") * m

  //   println(s"kilometre: $km")
  //   assert(km.multiplier == 1000)
  //   assert(km.quantity == Quantity.get("length").get)
  // }

  // test("can create a base unit by multiplying other unit by prefix") {
  //   val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

  //   val kg = Unido.create("kilogram", 1, mass, new SIPrefix("kilo"))
  //   val g = Unido.create("gram", 1/1000, mass)

  //   assert(kg.multiplier == 1)
  //   assert(kg.quantity == Quantity.get("mass").get)
  //   assert(g.multiplier == 1/1000)
  //   assert(g.quantity == Quantity.get("mass").get)
  // }

  test("can create prefixed units") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    SIPrefix.createUnits("metre", length)

    val km = Unido("kilometre")
    val m = Unido("metre")

    assert(km.multiplier == 1000)
    assert(km.quantity == Quantity.get("length").get)
    println(s"M: $m, ${m.multiplier}")
    assert(m.multiplier == 1)
    assert(m.quantity == Quantity.get("length").get)
  }

  test("can create prefixed units with custom unprefixed value") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    SIPrefix.createUnits("gram", mass, 1e-3)

    val kg = Unido("kilogram")
    val g = Unido("gram")

    assert(kg.multiplier == 1)
    assert(kg.quantity == Quantity.get("mass").get)
    println(s"G: $g, ${g.multiplier}")
    assert(g.multiplier == 1e-3)
    assert(g.quantity == Quantity.get("mass").get)
  }

}
