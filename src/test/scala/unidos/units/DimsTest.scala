package unidos

import unidos.units.Dims

class DimsTest extends munit.FunSuite {
  test("can compare dims") {
    assert(Dims(0, 1, 3, 2, 0, 0, 0) == Dims(0, 1, 3, 2, 0, 0, 0))
  }

  test("returns dimensionless dims") {
    val item = Dims.get("Dimensionless").get
    assert(item == Dims(0, 0, 0, 0, 0, 0, 0))
  }

  test("returns axis dims") {
    val item = Dims.get("Length").get
    assert(item == Dims(0, 1, 0, 0, 0, 0, 0))
  }

  test("makes one dimension dims") {
    val item = Dims.makeOneDimensionDims(1, 1)
    assert(item == Dims.get("Length").get)
  }

  test("creates multidimensional dims") {
    val item = Dims.name(Dims(-2, 1, 0, 0, 0, 0, 0), "acceleration")
    assert(item == Dims.get("acceleration").get)
  }

  test("can multiply dims") {
    val accel = Dims.name(Dims(-2, 1, 0, 0, 0, 0, 0), "acceleration")
    val time = Dims.get("Time").get

    assert(accel * time == Dims(-1, 1, 0, 0, 0, 0, 0))
  }

  test("can divide dims") {
    val length = Dims.get("Length").get
    val time = Dims.get("Time").get

    assert(length / time == Dims(-1, 1, 0, 0, 0, 0, 0))
  }

  test("can take power of dims") {
    val length = Dims.get("Length").get

    assert(Dims.pow(length, 2) == Dims(0, 2, 0, 0, 0, 0, 0))
  }

  test("can take root of dims") {
    val volume = Dims.name(Dims(0, 3, 0, 0, 0, 0, 0), "volume")
    val length = Dims.get("Length").get

    assert(Dims.root(volume, 3) == length)
  }


  test("can have multiple names") {
    val dims = Dims(0, 0, 0, 0, 0, 0, 0)
    Dims.name(dims, "dimensionless")
    Dims.name(dims, "plane angle")
    Dims.name(dims, "solid angle")

    assert(Dims.get("dimensionless").get == dims)
    assert(Dims.get("plane angle").get == dims)
    assert(Dims.get("solid angle").get == dims)
  }

  test("first name is default") {
    Dims.name(Dims(0, 1, 2, 3, 0, 0, 0), "test 1")
    Dims.name(Dims(0, 1, 2, 3, 0, 0, 0), "test 2")

    val name = Dims.get(Dims(0, 1, 2, 3, 0, 0, 0)).get
    assert(name == "test 1")
  }


}
