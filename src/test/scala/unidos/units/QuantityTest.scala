package unidos

import unidos.units.{Dims, Quantity}

class QuantityTest extends munit.FunSuite {
  override def beforeEach(context: BeforeEach): Unit = {
    Quantity.clear
  }

  test("returns dimensionless dims") {
    val Array(
      dimensionless,
      x
    ) = Quantity.createBaseQuantities(Array("dimensionless", "x"))

    assert(dimensionless.dims == Dims(0))
  }

  test("returns axis dims") {
    val Array(
      dimensionless,
      time,
      length,
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length"))

    assert(time.dims == Dims(1, 0))
    assert(length.dims == Dims(0, 1))
  }

  test("creates quantities") {
    val Array(
      dimensionless,
      time,
      length
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length"))

    val item = Quantity.create("acceleration", Dims(-2, 1))
    assert(item == Quantity.get("acceleration").get)
  }


  test("can have multiple names") {
    val Array(
      dimensionless
    ) = Quantity.createBaseQuantities(Array("dimensionless"))

    Quantity.create("plane angle", Dims())
    Quantity.create("solid angle", Dims())

    assert(Quantity.get("dimensionless").get.dims == Dims())
    assert(Quantity.get("plane angle").get.dims == Dims())
    assert(Quantity.get("solid angle").get.dims == Dims())
  }

  test("first name is default") {
    val Array(dimensionless, a, b, c, d) =
      Quantity.createBaseQuantities(
        Array("dimensionless", "a", "b", "c", "d")
      )

    Quantity.create("test 1", Dims(0, 1, 2, 3))
    Quantity.create("test 2", Dims(0, 1, 2, 3))

    val q = Quantity.get(Dims(0, 1, 2, 3))
    assert(q.name == "test 1")
  }

  test("can create by multiplying a quantity by another quantity") {
    val Array(
      dimensionless,
      time,
      length,
      mass
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length", "mass"))

    val force = Quantity.create("force", Dims(-2, 1, 1))
    val torque = Quantity.create("torque", force * length)

    assert(torque.dims == Dims(-2, 2, 1))
  }

  test("can create by dividing a quantity by another quantity") {
    val Array(
      dimensionless,
      time,
      length,
      mass
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length", "mass"))

    val torque = Quantity.create("torque", Dims(-2, 2, 1))
    val force = Quantity.create("force", torque / length)

    assert(force.dims == Dims(-2, 1, 1))
  }

  test("can create by raising a quantity to a power") {
    val Array(
      dimensionless,
      time,
      length,
      mass
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length", "mass"))

    val areaCheck = Quantity.pow(length, 2)

    assert(areaCheck.dims == Dims(0, 2, 0))
  }

  test("can create by taking a root of another quantity") {
    val Array(
      dimensionless,
      time,
      length,
      mass
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length", "mass"))

    val area = Quantity.create("area", Dims(0, 2, 0))
    val lengthCheck = Quantity.root(area, 2)

    assert(lengthCheck.dims == Dims(0, 1, 0))
  }




  test("operations can create new implicit quantities") {
    val Array(
      dimensionless,
      time,
      length,
      mass
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length", "mass"))
    val test = Quantity.create("test", Dims(-9, 9, 9))
    val x = test / length

    assert(x.dims == Dims(-9, 8, 9))

    assert(x.name == "time⁻⁹ length⁸ mass⁹")
  }

  test("multiple quantities can share a dimension") {
    val Array(
      dimensionless,
      time,
      length,
      mass
    ) = Quantity.createBaseQuantities(Array("dimensionless", "time", "length", "mass"))

    val `1` = Quantity.get("dimensionless").get
    val planeAngle = Quantity.create("plane angle", Dims(0, 0, 0))
    val solidAngle = Quantity.create("solid angle", Dims(0, 0, 0))

    assert(`1` != planeAngle)
    assert(`1` != solidAngle)
    assert(solidAngle != planeAngle)
  }

  test("basic quantity creation") {
    val Array(dimensionless, a, b, c, d, e, f, g, h) =
      Quantity.createBaseQuantities(
        Array("dimensionless", "a", "b", "c", "d", "e", "f", "g", "h")
      )

    assert(a.dims == Dims(1, 0, 0, 0, 0, 0, 0, 0))
    assert(b.dims == Dims(0, 1, 0, 0, 0, 0, 0, 0))
  }


}
