package unidos

import scala.collection.mutable.Queue

import unidos.shuntingyard.Parser


class ParserTest extends munit.FunSuite {
  def printArray(arr: Queue[String]): Unit = {
    for ( item <- arr ) {
      print(s"\"$item\" ")
    }
    println("")
  }
  test("addition") {
    assert(Parser.parse("2.34 + 2.1e3") sameElements Array("2.34", "2.1e3", "+"))
  }

  test("subtraction") {
    assert(Parser.parse("2.34 - 2.1e3") sameElements Array("2.34", "2.1e3", "-"))
  }

  test("multiplication *") {
    assert(Parser.parse("2.34 * 2.1e3") sameElements Array("2.34", "2.1e3", "*"))
  }

  test("multiplication ·") {
    assert(Parser.parse("2.34 · 2.1e3") sameElements Array("2.34", "2.1e3", "·"))
  }

  test("division") {
    assert(Parser.parse("2.34 / 2.1e3") sameElements Array("2.34", "2.1e3", "/"))
  }

  test("power".ignore) {
    assert(Parser.parse("2.34 ^ 2.1e3") sameElements Array("2.34", "2.1e3", "^"))
  }

  test("sqrt".ignore) {
    assert(Parser.parse("sqrt(2.34)") sameElements Array("2.34", "sqrt"))
  }

  test("log".ignore) {
    assert(Parser.parse("log(8, 2)") sameElements Array("8", "2", "log"))
  }

  test("implicit multiplication") {
    assert(Parser.parse("2.34 m") sameElements Array("2.34", "m", "·"))
    assert(Parser.parse("2.34 m^2") sameElements Array("2.34", "m", "2", "^", "·"))
    assert(Parser.parse("2.34 m m m") sameElements Array("2.34", "m", "·", "m", "·", "m", "·"))
  }

  test("addition and subtraction are done in order") {
    assert(Parser.parse("2 + 3 - 4") sameElements Parser.parse("(2 + 3) - 4"))
    assert(Parser.parse("2 - 3 + 4") sameElements Parser.parse("(2 - 3) + 4"))
  }

  test("multiplication and division are done in order") {
    assert(Parser.parse("2 · 3 / 4") sameElements Parser.parse("(2 · 3) / 4"))
    assert(Parser.parse("2 / 3 · 4") sameElements Parser.parse("(2 / 3) · 4"))
  }

  test("multiplication is done before addition") {
    assert(Parser.parse("2 + 3 · 4") sameElements Parser.parse("2 + (3 · 4)"))
    assert(Parser.parse("2 · 3 + 4") sameElements Parser.parse("(2 · 3) + 4"))
  }

  test("multiplication is done before subtraction") {
    assert(Parser.parse("2 - 3 · 4") sameElements Parser.parse("2 - (3 · 4)"))
    assert(Parser.parse("2 · 3 - 4") sameElements Parser.parse("(2 · 3) - 4"))
  }

  test("division is done before addition") {
    assert(Parser.parse("2 + 3 / 4") sameElements Parser.parse("2 + (3 / 4)"))
    assert(Parser.parse("2 / 3 + 4") sameElements Parser.parse("(2 / 3) + 4"))
  }

  test("division is done before subtraction") {
    assert(Parser.parse("2 - 3 / 4") sameElements Parser.parse("2 - (3 / 4)"))
    assert(Parser.parse("2 / 3 - 4") sameElements Parser.parse("(2 / 3) - 4"))
  }


  test("implicit multiplication is done before division") {
    assert(Parser.parse("2 m / 4") sameElements Parser.parse("(2 · m) / 4"))
    assert(Parser.parse("2 / 3 m") sameElements Parser.parse("2 / (3 · m)"))
  }

  test("exponentiation is done before multiplication") {
    assert(Parser.parse("2 · 3 ^ 4") sameElements Parser.parse("2 · (3 ^ 4)"))
    assert(Parser.parse("2 ^ 3 · 4") sameElements Parser.parse("(2 ^ 3) · 4"))
  }

  test("exponentiation is done before division") {
    assert(Parser.parse("2 / 3 ^ 4") sameElements Parser.parse("2 / (3 ^ 4)"))
    assert(Parser.parse("2 ^ 3 / 4") sameElements Parser.parse("(2 ^ 3) / 4"))
  }

  test("exponentiation is done before implicit multiplication") {
    assert(Parser.parse("4 m ^ 2") sameElements Parser.parse("4 · (m ^ 2)"))
    assert(Parser.parse("2 ^ 3 m") sameElements Parser.parse("(2 ^ 3) · m"))
  }

  test("exponentiation is right associative") {
    assert(Parser.parse("4 ^ 3 ^ 2 ^ 1") sameElements Parser.parse("4 ^ (3 ^ (2 ^ 1))"))
  }


  test("one parameter functions") {
    assert(Parser.parse("sqrt(4)") sameElements Array("4", "sqrt"))
    assert(Parser.parse("sqrt 4") sameElements Array("4", "sqrt"))
  }

}
