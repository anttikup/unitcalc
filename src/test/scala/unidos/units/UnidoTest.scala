package unidos

import unidos.units.{Dims, Quantity, Unido, Unidos}



class UnidoTest extends munit.FunSuite {
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



  test("can create") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val unit = Unido.create("metre", 1, length)
  }

  test("apply returns same object for same name") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    Unido.create("metre", 1, length)
    val metre1 = Unido("metre")
    val metre2 = Unido("metre")

    assert(metre1 == metre2)
  }

  test("same parameters create same object") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val metre1 = Unido.create("metre", 1, length)
    val metre2 = Unido.create("metre", 1, length)

    assert(metre1 == metre2)
  }

  test("same parameters create same object (baseUnit)") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val metre1 = Unido.create("metre", Quantity.baseUnitOf("length"))
    val metre2 = Unido.create("metre", Quantity.baseUnitOf("length"))

    assert(metre1 == metre2)
  }

  test("same name with different unit throws error") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val metre1 = Unido.create("metre", Quantity.baseUnitOf("length"))

    intercept[java.lang.Error] {
      val metre2 = Unido.create("metre", Quantity.baseUnitOf("time"))
    }
  }

  test("sets 1 unit as default") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val metre = Unido.create("metre", 1, length)
    val default = Unidos.getDefaultUnitForQuantity("length")

    assert(metre == default)
  }

  test("can create a unit by multiplying other unit by scalar") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val km = Unido.create("kilometre", m * 1000)

    assert(km.multiplier == 1000)
    assert(km.quantity == Quantity.get("length").get)
  }

  test("can create a unit by dividing other unit by scalar") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val mm = Unido.create("millimetre", m / 1000)

    assert(mm.multiplier == 0.001)
    assert(mm.quantity == Quantity.get("length").get)
  }

  test("can create a unit by multiplying an unit by another unit ") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val force = Quantity.create("force", Dims(-2, 1, 1, 0, 0, 0, 0))
    val torque = Quantity.create("torque", Dims(-2, 2, 1, 0, 0, 0, 0))
    val N = Unido.create("newton", 1, force)
    val m = Unido.create("metre", 1, length)
    val Nm = Unido.create("newton-metre", N * m)

    assert(Nm.multiplier == 1)
    assert(Nm.quantity == torque)
  }

  test("multiplying a unit by a dimensionless unit keeps the original unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val force = Quantity.create("force", Dims(-2, 1, 1, 0, 0, 0, 0))
    val `1` = Unido.create("1", Quantity.baseUnitOf("dimensionless"))
    val N = Unido.create("newton", Quantity.baseUnitOf("force"))

    val result = `1` * N

    assert(result.quantity == force)
    assert(result.multiplier == 1)
    assert(result.name == Some("newton"))
  }

  test("multiplying a unit by a dimensionless unit keeps the original unit (reverse)") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val force = Quantity.create("force", Dims(-2, 1, 1, 0, 0, 0, 0))
    val `1` = Unido.create("1", Quantity.baseUnitOf("dimensionless"))
    val N = Unido.create("newton", Quantity.baseUnitOf("force"))

    val result = N * `1`

    assert(result.quantity == force)
    assert(result.multiplier == 1)
    assert(result.name == Some("newton"))
  }

  test("dividing a unit by a dimensionless unit keeps the original unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val force = Quantity.create("force", Dims(-2, 1, 1, 0, 0, 0, 0))
    val `1` = Unido.create("1", Quantity.baseUnitOf("dimensionless"))
    val N = Unido.create("newton", Quantity.baseUnitOf("force"))

    val result = N / `1`

    assert(result.quantity == force)
    assert(result.multiplier == 1)
    assert(result.name == Some("newton"))
  }

  test("can create an inverted unit by dividing dimensionless with a unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val `1` = Unido.create("1", 1, dimensionless)
    val s = Unido.create("second", 1, time)

    var result = `1`/s

    assert(result.multiplier == 1)
    assert(result.name == Some("second⁻¹"))
  }


  test("multiple quantities can share a dimension, but have their own units") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val planeAngle = Quantity.create("plane angle", Dims(0, 0, 0, 0, 0, 0, 0))
    val solidAngle = Quantity.create("solid angle", Dims(0, 0, 0, 0, 0, 0, 0))

    val `1` = Unido.create("1", Quantity.baseUnitOf("dimensionless"))
    val rad = Unido.create("radian", Quantity.baseUnitOf("plane angle"))
    val sr = Unido.create("steradian", Quantity.baseUnitOf("solid angle"))

    assert(`1` != rad)
    assert(rad != sr)
    assert(sr != `1`)
  }

  test("operations can create new implicit units") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", Quantity.baseUnitOf("time"))
    val m = Unido.create("metre", Quantity.baseUnitOf("length"))
    val kg = Unido.create("kilogram", Quantity.baseUnitOf("mass"))

    val test = Quantity.create("test", Dims(-9, 9, 9, 0, 0, 0, 0))
    val xxx = Unido.create("xxx", Quantity.baseUnitOf("test"))

    val x = xxx / m

    assert(x.name == Some("second⁻⁹ metre⁸ kilogram⁹"))
  }



}
