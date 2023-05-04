package unidos

import unidos.units.{Dims, Quantity}


class QuantityTest extends munit.FunSuite {

  test("returns dimensionless dims") {
    val item = Quantity.get("dimensionless").get
    assert(item == Dims(0, 0, 0, 0, 0, 0, 0))
  }

  test("returns axis dims") {
    val item = Quantity.get("length").get
    assert(item == Dims(0, 1, 0, 0, 0, 0, 0))
  }

  test("creates quantities") {
    val item = Quantity.create("acceleration", Dims(-2, 1, 0, 0, 0, 0, 0))
    assert(item == Quantity.get("acceleration").get)
  }


  test("can have multiple names") {
    val dims = Dims(0, 0, 0, 0, 0, 0, 0)
    Quantity.create("dimensionless", dims)
    Quantity.create("plane angle", dims)
    Quantity.create("solid angle", dims)

    assert(Quantity.get("dimensionless").get == dims)
    assert(Quantity.get("plane angle").get == dims)
    assert(Quantity.get("solid angle").get == dims)
  }

  test("first name is default") {
    Quantity.create("test 1", Dims(0, 1, 2, 3, 0, 0, 0))
    Quantity.create("test 2", Dims(0, 1, 2, 3, 0, 0, 0))

    val name = Quantity.get(Dims(0, 1, 2, 3, 0, 0, 0))
    assert(name == "test 1")
  }

  test("can create by multiplying a quantity by another quantity") {
    val force = Quantity.create("force", Dims(-2, 1, 1, 0, 0, 0, 0))
    val length = Quantity.get("length").get
    val torque = Quantity.create("torque", force * length)

    assert(torque == Dims(-2, 2, 1, 0, 0, 0, 0))
  }

  test("can create by dividing a quantity by another quantity") {
    val torque = Quantity.create("torque", Dims(-2, 2, 1, 0, 0, 0, 0))
    val length = Quantity.get("length").get
    val force = Quantity.create("force", torque / length)

    assert(force == Dims(-2, 1, 1, 0, 0, 0, 0))
  }

  test("operations can create new implicit quantities") {
    val test = Quantity.create("test", Dims(-9, 9, 9, 0, 0, 0, 0))
    val length = Quantity.get("length").get
    val x = test / length

    assert(x == Dims(-9, 8, 9, 0, 0, 0, 0))

    assert(Quantity.get(x) == "time⁻⁹ length⁸ mass⁹")
  }

  test("multiple quantities can share a dimension") {
    val `1` = Quantity.get("dimensionless").get
    val planeAngle = Quantity.create("plane angle", Dims(0, 0, 0, 0, 0, 0, 0))
    val solidAngle = Quantity.create("solid angle", Dims(0, 0, 0, 0, 0, 0, 0))

    assert(`1` == planeAngle)
    assert(`1` == solidAngle)
  }


}
