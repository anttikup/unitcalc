package unidos

import unidos.units.{Quantity, Unitful, Unido, Dims}



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
    println(result)
    assert(result.unit.name == Some("second¹ metre¹"))
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
    assert(result.unit.name == Some("second⁻¹ metre¹"))
  }

  test("can invert by dividing one with it") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val s = Unido.create("second", 1, time)

    val value1 = Unitful(1)
    val value2 = Unitful(2, s)
    val result = value1 / value2

    assert(result.amount == 0.5)
    assert(result.unit.name == Some("second⁻¹"))
  }

  test("unit is kept on addition when multiple quantities shares the same dimension") {
    val Array(dimensionless, time, length, mass, _*) = createBasicDims : @unchecked

    val planeAngle = Quantity.create("plane angle", Dims(0, 0, 0, 0, 0, 0, 0))
    val solidAngle = Quantity.create("solid angle", Dims(0, 0, 0, 0, 0, 0, 0))

    val rad = Unido.create("radian", Quantity.baseUnitOf("plane angle"))

    val value1 = Unitful(0.3, rad)
    val value2 = Unitful(0.2, rad)
    val result = value1 + value2

    println(s"result $result")

    assert(result.amount == 0.5)
    assert(result.unit == rad)

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

}
