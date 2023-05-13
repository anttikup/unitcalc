package unidos
import unidos.units.{CompoundName, NamePart}




class CompoundNameTest extends munit.FunSuite {
  test("can create simple") {
    val name = new CompoundName("metre" -> 1)
    assert(name.toString == "metre")
  }

  test("can create division") {
    val name = new CompoundName("metre" -> 1, "second" -> -1)
    println(name)
    assert(name.toString == "metre/second")
  }

  test("can create inverted") {
    val name = new CompoundName("second" -> -1)
    println(name)
    assert(name.toString == "1/second")
  }

  test("can create simple power") {
    val name = new CompoundName("metre" -> 2)
    assert(name.toString == "metre²")
  }

  test("can create inverted power") {
    val name = new CompoundName("metre" -> -2)
    assert(name.toString == "1/metre²")
  }

  test("can create division with power") {
    val name = new CompoundName("metre" -> 1, "second" -> -2)
    println(name)
    assert(name.toString == "metre/second²")
  }

  test("can create division 2") {
    val name = new CompoundName("kilometre" -> 1, "hour" -> -1)
    println(name)
    assert(name.toString == "kilometre/hour")
  }

  test("can invert") {
    val name = new CompoundName("kilometre" -> 1, "hour" -> -1)
    println(name.invert)
    assert(name.invert.toString == "hour/kilometre")
  }

  test("can multiply") {
    val name1 = new CompoundName("kilometre" -> 1, "hour" -> -1)
    val name2 = new CompoundName("centimetre" -> 1, "kilogram" -> -1)

    val result = name1 * name2

    println(result)
    assert(result.toString == "centimetre kilometre/hour/kilogram")
  }

  test("can divide") {
    val name1 = new CompoundName("kilometre" -> 1, "hour" -> 1)
    val name2 = new CompoundName("centimetre" -> 1, "kilogram" -> 1)

    val result = name1 / name2

    println(result)
    assert(result.toString == "hour kilometre/centimetre/kilogram")
  }
}
