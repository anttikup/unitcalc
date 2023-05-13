package unidos

import unidos.units.{CompoundName, Dims, Quantity, Unido, Unidos}




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

  test("dimensionless returns 1") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val unit = Unido("1")

    assert(unit.quantity == dimensionless)
    assert(unit.name == Some("1"))
  }


  test("can create") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val unit = Unido.create("metre", 1, length)
  }

  test("can create unitless") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val unit = Unido.create("1", 1, dimensionless)
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
    assert(result.name == Some("1/second"))
  }


  test("can raise a unit to an integer power") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val dm = Unido.create("decimetre", m * 0.1)

    val result = Unido.pow(dm, 2)

    assert(result.quantity.name == "length²")
    assert(result.name == Some("metre²"))
    assert(result.multiplier == Math.pow(0.1, 2))

  }



  test("can create a new unit by raising a unit to an integer power") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val volume = Quantity.create("volume", Quantity.pow(length, 3))
    val m = Unido.create("metre", 1, length)
    val dm = Unido.create("decimetre", m * 0.1)
    val cm = Unido.create("centimetre", m * 0.01)
    val `m³` = Unido.create("metre³", Unido.pow(m, 3))
    val `dm³` = Unido.create("decimetre³", Unido.pow(dm, 3))
    val `cm³` = Unido.create("centimetre³", Unido.pow(cm, 3))

    val L = Unido.create("litre", `dm³`)
    assert(L.quantity == Quantity.get("volume").get)

    val ml = Unido.create("millilitre", L * 1e-3 )
    val kl = Unido.create("kilolitre", L * 1e3 )

    assert(ml === `cm³`)
    assert(kl === `m³`)

  }

  test("can create an inverted unit by rising it to the power of negative one") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", 1, time)

    var result = Unido.pow(s, -1)

    assert(result.multiplier == 1)
    assert(result.name == Some("second⁻¹"))
  }

  test("can create inverted unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val freq = Quantity.create("frequency", dimensionless/time)

    val s = Unido.create("second", 1, time)
    val Hz = Unido.create("hertz", Unido.pow(s, -1))

    assert(Hz.multiplier == 1)
    assert(Hz.name == Some("hertz"))
    assert(Hz.quantity.name == "frequency")
  }

  test("can create a new unit by taking an integer root of another unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val timearea = Quantity.create("timearea", Dims(2, 0, 0, 0, 0, 0, 0))
    val `m²` = Unido.create("square second", 1, timearea)

    val s = Unido.create("second", Unido.root(`m²`, 2))

    assert(s.multiplier == 1)
    assert(s.quantity == time)

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

  test("operations can create new units, division") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", Quantity.baseUnitOf("time"))
    val m = Unido.create("metre", Quantity.baseUnitOf("length"))
    val kg = Unido.create("kilogram", Quantity.baseUnitOf("mass"))

    val test = Quantity.create("test", Dims(-9, 9, 9, 0, 0, 0, 0))
    val xxx = Unido.create("xxx", Quantity.baseUnitOf("test"))

    val x = xxx / m

    println(x)
    assert(x.name == Some("xxx/metre"))
  }

  test("operations can create new compound units, division 2") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", Quantity.baseUnitOf("time"))
    val m = Unido.create("metre", Quantity.baseUnitOf("length"))

    val ds = Unido.create("decisecond", s / 10)
    val hm = Unido.create("hectometre", m * 100)

    val dsphm = ds / hm

    println(s"RESULT: $dsphm")
    assert(dsphm.multiplier == 0.001)
    assert(dsphm.name == Some("decisecond/hectometre"))
  }

  test("operations can create new compound units, multiplication") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val kg = Unido.create("kilogram", Quantity.baseUnitOf("mass"))
    val m = Unido.create("metre", Quantity.baseUnitOf("length"))

    val kgm = kg * m

    println(s"RESULT: $kgm")
    assert(kgm.multiplier == 1)
    assert(kgm.name == Some("kilogram metre"))
  }

  test("can create new compound name units") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val speed = Quantity.create("speed", length / time)
    val mps = Unido.create(CompoundName("metre" -> 1, "second" -> -1), 1, speed)

    assert(mps.multiplier == 1)
    assert(mps.name == Some("metre/second"))
    assert(Unido("metre/second") === mps)
  }

  test("can create new compound name units, alt") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", Quantity.baseUnitOf("time"))
    val m = Unido.create("metre", Quantity.baseUnitOf("length"))
    val mps = Unido.create(CompoundName("metre" -> 1, "second" -> -1), m/s)

    assert(mps.multiplier == 1)
    assert(mps.name == Some("metre/second"))
    assert(Unido("metre/second") === mps)
  }

  test("operations can create new implicit compound units") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", 1, time)
    val m = Unido.create("metre", 1, length)
    val `m/s` = m/s

    assert(`m/s`.multiplier == 1)
    assert(`m/s`.name == Some("metre/second"))
    assert(Unido("metre/second") === `m/s`)
  }


}
