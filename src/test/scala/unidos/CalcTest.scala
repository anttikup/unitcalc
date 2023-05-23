package unidos

import unidos.Calc
import unidos.units.List
import unidos.units.{CompoundName, Unido, Unitful, Quantity, Unidos}


class CalcTest extends munit.FunSuite {
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

  override def beforeEach(context: BeforeEach): Unit = {
    Quantity.clear
    createBasicDims
    Unidos.clear
    List.load
    Calc.preload
  }

  test("unitless addition") {
    val result = Calc.calc("2 + 3")
    assert(result === Unitful(5))
  }

  test("addition with units") {
    val result = Calc.calc("2 m + 3 m")
    assert(result === Unitful(5, Unido("metre")))
  }

  test("unitless subtraction") {
    val result = Calc.calc("3 - 2")
    assert(result === Unitful(1))
  }

  test("subtraction with units") {
    val result = Calc.calc("3 m - 2 m")
    assert(result === Unitful(1, Unido("metre")))
  }

  test("unitless multiplication") {
    val result = Calc.calc("2 · 3")
    assert(result === Unitful(6))
  }

  test("multiplication with unit with itself") {
    val result = Calc.calc("2 m · 3 m")
    assert(result === Unitful(6, Unido.pow(Unido("metre"), 2)))
  }

  test("unitless division") {
    val result = Calc.calc("6 / 2")
    assert(result === Unitful(3))
  }

  test("division with units") {
    val result = Calc.calc("6 m / 2 m")
    assert(result === Unitful(3))
  }

  test("power with units") {
    val result = Calc.calc("(2 m) ^ 2")
    assert(result === Unitful(4, Unido.pow(Unido("metre"), 2)))
  }

  test("power of unit") {
    val result = Calc.calc("2 m^2")
    assert(result === Unitful(2, Unido.pow(Unido("metre"), 2)))
  }

  test("power is right associative") {
    assert(Calc.calc("2 ^ 3 ^ 4") === Calc.calc("2 ^ (3 ^ 4)"))
  }

  test("unitless square root") {
    assert(Calc.calc("sqrt(4)") === Calc.calc("2"))
    assert(Calc.calc("sqrt 4") === Calc.calc("2"))
  }

  test("roots with units") {
    assert(Calc.calc("sqrt(4 m^2)") === Calc.calc("2 m"))
    assert(Calc.calc("cbrt(27 m^3)") === Calc.calc("3 m"))
  }

  // TODO
  test("logarithm with units".ignore) {
    assert(Calc.calc("log(4 m^2, 2 m)") === Calc.calc("2"))
  }

  test("multiplication with unit with itself many times") {
    val result = Calc.calc("2 m · 3 m · 4 m · 5 m")
    assert(result === Unitful(120, Unido.pow(Unido("metre"), 4)))
  }

  test("one dissappears if other units are present") {
    val result = Calc.calc("25 m^2")
    assert(result.unit.name.toString == "metre²")
  }

  test("can convert units") {
    Env.setSymbol("cm", "centimetre")
    val result = Calc.calc("11 m ? cm")
    assert(result.amount == 1100)
    assert(result.unit.name.toString == "centimetre")
  }


}

// 2 dm ^ 4
//    2	metre⁴	length⁴
// 2 dm³ = x m³; 2 dm = 0.2 m; 4 dm² = (2 dm)² = (2*0.1 m)² = (0.2 m)² = 0.04 m² = (sqrt(4) · 0.1 m)² = (4 · 0.1²) m²
