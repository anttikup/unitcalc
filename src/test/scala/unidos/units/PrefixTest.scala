package unidos

import unidos.units.{Dims, Quantity, Unido, Unidos}
import unidos.units.prefix.{IECPrefix, SIPrefix}



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
    val pm = Unido("picometre")

    assert(km.multiplier == 1000)
    assert(km.quantity == Quantity.get("length").get)

    assert(m.multiplier == 1)
    assert(m.quantity == Quantity.get("length").get)

    assert(pm.multiplier == 1e-12)
    assert(pm.quantity == Quantity.get("length").get)

    assert(Quantity.baseUnitOf("length") == m)
  }

  test("can create prefixed units with custom unprefixed value") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    SIPrefix.createUnits("gram", mass, 1e-3)

    val kg = Unido("kilogram")
    val g = Unido("gram")
    val pg = Unido("picogram")

    assert(kg.multiplier == 1)
    assert(kg.quantity == Quantity.get("mass").get)

    assert(g.multiplier == 1e-3)
    assert(g.quantity == Quantity.get("mass").get)

    assert(pg.multiplier == g.multiplier * 1e-12)
    assert(pg.quantity == Quantity.get("mass").get)

    assert(Quantity.baseUnitOf("mass") == kg)
  }

  test("can create non-SI prefixed units") {
    val Array(dimensionless, binary) = Quantity.createBaseQuantities(
      Array(
        "dimensionless",
        "binary",
      )
    )

    IECPrefix.createUnits("byte", binary)

    val TiB = Unido("tebibyte")
    val B = Unido("byte")

    assert(TiB.multiplier == Math.pow(2, 40))
    assert(TiB.quantity == Quantity.get("binary").get)

    assert(B.multiplier == 1)
    assert(B.quantity == Quantity.get("binary").get)

    assert(Quantity.baseUnitOf("binary") == B)
  }


}
