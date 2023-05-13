package unidos

import unidos.units.{Quantity, Unitful, Unido, Dims, CompoundUnido}
import unidos.units.{Util}



class UnitfulTest extends munit.FunSuite {
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

    val m = Unido.create("metre", 1, length)
    val value = Unitful(9, m)

    assert(value.amount == 9)
    assert(value.unit == m)
  }

  test("can create scalar valued") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val value = Unitful(9)

    assert(value.amount == 9)
    assert(value.unit == Unido(1, dimensionless))
  }

  test("can add same unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val value1 = Unitful(9, m)
    val value2 = Unitful(11, m)
    val result = value1 + value2

    assert(result.amount == 20)
    assert(result.unit == m)
  }

  test("can add same quantity, different unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val km = Unido.create("kilometre", m * 1000)
    val value1 = Unitful(900, m)
    val value2 = Unitful(0.1, km)
    val result = value1 + value2

    assert(result.amount == 1000)
    assert(result.unit == m)
  }

  test("can subtract same unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val value1 = Unitful(9, m)
    val value2 = Unitful(1, m)
    val result = value1 - value2

    assert(result.amount == 8)
    assert(result.unit == m)
  }

  test("can subtract same quantity, different unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val km = Unido.create("kilometre", m * 1000)
    val value1 = Unitful(900, m)
    val value2 = Unitful(0.1, km)
    val result = value1 - value2

    assert(result.amount == 800)
    assert(result.unit == m)
  }

  test("can multiply by scalar") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val value = Unitful(900, m)
    val result = value * 0.1

    assert(result.amount == 90)
    assert(result.unit == m)
  }

  test("can multiply by another unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val s = Unido.create("second", 1, time)

    val value1 = Unitful(10, m)
    val value2 = Unitful(2, s)
    val result = value1 * value2

    assert(result.amount == 20)
    assert(result.unit.name == "metre second")
  }

  test("can divide by scalar") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val value = Unitful(900, m)
    val result = value / 2

    assert(result.amount == 450)
    assert(result.unit == m)
  }

  test("can divide by another unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val s = Unido.create("second", 1, time)

    val value1 = Unitful(10, m)
    val value2 = Unitful(2, s)
    val result = value1 / value2

    assert(result.amount == 5)
    assert(result.unit.name == "metre/second")
  }

  test("can raise to an integer power") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val volume = Quantity.create("volume", Quantity.pow(length, 3))

    val m = Unido.create("metre", 1, length)
    val `m³` = Unido.create("metre³", 1, volume)
    val value = Unitful(10, m)
    val result = Unitful.pow(value, 3)

    assert(result.amount == 1000)
    assert(result.unit == `m³`)
  }

  test("can raise unit to an integer power") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val dm = Unido.create("decimetre", m * 0.1)

    val value = Unitful(2, dm)
    val result = Unitful.pow(value, 2)

    assert(result.unit.name == "metre²")
    assert(Util.almostEquals(result.amount, 4))
  }

  test("can raise value with dimensionless unit to any power") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val rad = Unido.create("rad", 1, dimensionless)
    val value = Unitful(0.234, rad)
    val result = Unitful.pow(value, 0.123)

    assert(result.amount != 0)
    assert(result.unit == rad)
  }

  test("can take integer root") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val volume = Quantity.create("volume", Quantity.pow(length, 3))
    val m = Unido.create("metre", 1, length)
    val `m³` = Unido.create("metre³", 1, volume)

    val value = Unitful(1000, `m³`)
    val result = Unitful.root(value, 3)

    assert(Util.almostEquals(result.amount, 10))
    assert(result.unit == m)
  }

  test("can take any root of dimensionless unit") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val rad = Unido.create("rad", 1, dimensionless)
    val value = Unitful(0.234, rad)
    val result = Unitful.root(value, 0.123)

    assert(result.amount != 0)
    assert(result.unit == rad)
  }


  test("can invert by dividing one with it") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", 1, time)

    val value1 = Unitful(1)
    val value2 = Unitful(2, s)
    val result = value1 / value2

    assert(result.amount == 0.5)
    assert(result.unit.name == "1/second")
  }

  test("unit is kept on addition when multiple quantities shares the same dimension") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val planeAngle = Quantity.create("plane angle", Dims(0, 0, 0, 0, 0, 0, 0))
    val solidAngle = Quantity.create("solid angle", Dims(0, 0, 0, 0, 0, 0, 0))

    val rad = Unido.create("radian", Quantity.baseUnitOf("plane angle"))

    val value1 = Unitful(0.3, rad)
    val value2 = Unitful(0.2, rad)
    val result = value1 + value2

    assert(result.amount == 0.5)
    assert(result.unit == rad)

  }

  test("unit is kept on multiplication by scalar Unitful") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", 1, time)

    val value1 = Unitful(2)
    val value2 = Unitful(2, s)
    val result = value1 * value2

    assert(result.amount == 4)
    assert(result.unit.name == "second")
  }

  test("dimensionless unit is kept on multiplication by scalar Unitful") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked
    val planeAngle = Quantity.create("plane angle", Dims(0, 0, 0, 0, 0, 0, 0))

    val rad = Unido.create("radian", 1, planeAngle)

    val value1 = Unitful(2)
    val value2 = Unitful(0.2, rad)
    val result = value1 * value2

    assert(result.amount == 0.4)
    assert(result.unit.name == "radian")
  }

  test("throws when quantities differ even if they share the same dimension") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val planeAngle = Quantity.create("plane angle", Dims(0, 0, 0, 0, 0, 0, 0))
    val solidAngle = Quantity.create("solid angle", Dims(0, 0, 0, 0, 0, 0, 0))

    val rad = Unido.create("radian", Quantity.baseUnitOf("plane angle"))
    val sr = Unido.create("steradian", Quantity.baseUnitOf("solid angle"))

    val value1 = Unitful(0.3, rad)
    val value2 = Unitful(0.2, sr)

    intercept[java.lang.Error] {
      val result = value1 + value2
    }
  }


  test("litres and m³ match each other") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val volume = Quantity.create("volume", Quantity.pow(length, 3))

    val m = Unido.create("metre", 1, length)
    val dm = Unido.create("decimetre", m * 0.1)
    val cm = Unido.create("centimetre", m * 0.01)

    val `m³` = Unido.create("metre³", Unido.pow(m, 3))
    val `dm³` = Unido.create("decimetre³", Unido.pow(dm, 3))
    val `cm³` = Unido.create("centimetre³", Unido.pow(cm, 3))

    val L = Unido.create("litre", `dm³`)
    val ml = Unido.create("millilitre", L * 1e-3 )
    val cl = Unido.create("centilitre", L * 1e-2 )
    val dl = Unido.create("decilitre", L * 1e-1 )
    val dal = Unido.create("decalitre", L * 1e1 )
    val hl = Unido.create("hectolitre", L * 1e2 )
    val kl = Unido.create("kilolitre", L * 1e3 )

    assert(Unitful(1, L) === Unitful(1, `dm³`))
    assert(Unitful(1, L) === Unitful(1000, `cm³`))
    assert(Unitful(1, dl) === Unitful(100, `cm³`))
    assert(Unitful(1, hl) === Unitful(100, `dm³`))
    assert(Unitful(1, dl) === Unitful(100, `cm³`))
    assert(Unitful(1, cl) === Unitful(10, `cm³`))
    assert(Unitful(1, ml) === Unitful(1, `cm³`))
  }


  test("currency") {
    val Array(dimensionless, currency) = Quantity.createBaseQuantities(
      Array(
        "dimensionless",
        "currency",
      )
    )

    val `€` = Unido.create("euro", 1, currency)
    val `¢` = Unido.create("cent", `€` * 0.01)

    assert(Unitful(1, `€`) + Unitful(50, `¢`) === Unitful(1.5, `€`))
    assert(Unitful(1, `€`) + Unitful(50, `¢`) === Unitful(150, `¢`))
  }


  test("can create compound units, division") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val s = Unido.create("second", 1, time)

    val result = Unitful(1, m) / Unitful(1, s)

    assert(result.unit.name == "metre/second")

  }

  test("can create compound units, multiplication") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val m = Unido.create("metre", 1, length)
    val s = Unido.create("second", 1, time)

    val result = Unitful(1, m) * Unitful(1, s)

    assert(result.unit.name == "metre second")

  }
}
