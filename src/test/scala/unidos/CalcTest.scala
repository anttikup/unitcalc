package unidos

import unidos.Calc
import unidos.units.List
import unidos.units.{Unido, Unitful}


class CalcTest extends munit.FunSuite {
  override def beforeAll(): Unit = {
    List
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

  test("multiplication with units") {
    val result = Calc.calc("2 m · 3 m")
    assert(result === Unitful(6, Unido("square metre")))
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
    assert(result === Unitful(4, Unido("square metre")))
  }

  test("power of unit") {
    val result = Calc.calc("2 m^2")
    assert(result === Unitful(2, Unido("square metre")))
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

}

// 2 dm ^ 4
//    2	metre⁴	length⁴
// 2 dm³ = x m³; 2 dm = 0.2 m; 4 dm² = (2 dm)² = (2*0.1 m)² = (0.2 m)² = 0.04 m² = (sqrt(4) · 0.1 m)² = (4 · 0.1²) m²
