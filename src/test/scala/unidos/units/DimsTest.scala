package unidos

import unidos.units.Dims


class DimsTest extends munit.FunSuite {
  test("can compare dims") {
    assert(Dims(0, 1, 3, 2, 0, 0, 0) == Dims(0, 1, 3, 2, 0, 0, 0))
  }

  test("makes one dimension dims") {
    Dims.numberOfDims = 7
    val item = Dims.makeOneDimensionDims(1, 1)
    assert(item == Dims(0, 1, 0, 0, 0, 0, 0))
  }

  test("can multiply dims") {
    Dims.numberOfDims = 7
    val accel = Dims(-2, 1, 0, 0, 0, 0, 0)
    val time = Dims(1, 0, 0, 0, 0, 0, 0)

    assert(accel * time == Dims(-1, 1, 0, 0, 0, 0, 0))
  }

  test("can divide dims") {
    val length = Dims(0, 1, 0, 0, 0, 0, 0)
    val time = Dims(1, 0, 0, 0, 0, 0, 0)

    assert(length / time == Dims(-1, 1, 0, 0, 0, 0, 0))
  }

  test("can take power of dims") {
    val length = Dims(0, 1, 0, 0, 0, 0, 0)

    assert(Dims.pow(length, 2) == Dims(0, 2, 0, 0, 0, 0, 0))
  }

  test("can take root of dims") {
    val volume = Dims(0, 3, 0, 0, 0, 0, 0)
    val length = Dims(0, 1, 0, 0, 0, 0, 0)

    assert(Dims.root(volume, 3) == length)
  }

  test("isElementary returns true if only one number is set and it is one") {
    assert(Dims(1, 0, 0, 0, 0, 0, 0).isElementary == true)
    assert(Dims(0, 1, 0, 0, 0, 0, 0).isElementary == true)
    assert(Dims(0, 3, 0, 0, 0, 0, 0).isElementary == false)
    assert(Dims(0, 1, 1, 0, 0, 0, 0).isElementary == false)
  }

}
