package unidos

import unidos.shuntingyard.tokenizer.Tokenizer


class TokenizerTest extends munit.FunSuite {
  def printArray(arr: Array[String]): Unit = {
    for ( item <- arr ) {
      print(s"\"$item\" ")
    }
    println("")
  }

  // Helper function to add quotes around string
  def q(str: String) =
    s"\"$str\""




  // NUMBERS

  test("numbers: integers") {
    val result = Tokenizer.tokenize("5 879")
    assert(result sameElements Array("5", "879"))
  }

  test("numbers: full floats") {
    val result = Tokenizer.tokenize("2.3 0.878 456.89")
    assert(result sameElements Array("2.3", "0.878", "456.89"))
  }

  test("numbers: floats with omitted 0") {
    val result = Tokenizer.tokenize(".5 .802")
    assert(result sameElements Array(".5", ".802"))
  }

  test("numbers: scientific with integer mantissa, no-sign in exponent, small e") {
    val result = Tokenizer.tokenize("3e2 300e2 3e20 300e20")
    assert(result sameElements Array("3e2", "300e2", "3e20", "300e20"))
  }

  test("numbers: scientific with integer mantissa, plus in exponent, small e") {
    val result = Tokenizer.tokenize("3e+2 300e+2 3e+20 300e+20")
    assert(result sameElements Array("3e+2", "300e+2", "3e+20", "300e+20"))
  }

  test("numbers: scientific with integer mantissa, minus in exponent, small e") {
    val result = Tokenizer.tokenize("3e-2 300e-2 3e-20 300e-20")
    assert(result sameElements Array("3e-2", "300e-2", "3e-20", "300e-20"))
  }

  test("numbers: scientific with integer mantissa, no-sign in exponent, big E") {
    val result = Tokenizer.tokenize("3E2 300E2 3E20 300E20")
    assert(result sameElements Array("3E2", "300E2", "3E20", "300E20"))
  }

  test("numbers: scientific with integer mantissa, plus in exponent, big E") {
    val result = Tokenizer.tokenize("3E+2 300E+2 3E+20 300E+20")
    assert(result sameElements Array("3E+2", "300E+2", "3E+20", "300E+20"))
  }

  test("numbers: scientific with integer mantissa, minus in exponent, big E") {
    val result = Tokenizer.tokenize("3E-2 300E-2 3E-20 300E-20")
    assert(result sameElements Array("3E-2", "300E-2", "3E-20", "300E-20"))
  }


  test("numbers: scientific with float mantissa, no-sign in exponent, small e") {
    val result = Tokenizer.tokenize("2.5e2 30.12e2 2.5e20 30.12e20")
    assert(result sameElements Array("2.5e2", "30.12e2", "2.5e20", "30.12e20"))
  }

  test("numbers: scientific with float mantissa, plus in exponent, small e") {
    val result = Tokenizer.tokenize("2.5e+2 30.12e+2 2.5e+20 30.12e+20")
    assert(result sameElements Array("2.5e+2", "30.12e+2", "2.5e+20", "30.12e+20"))
  }

  test("numbers: scientific with float mantissa, minus in exponent, small e") {
    val result = Tokenizer.tokenize("2.5e-2 30.12e-2 2.5e-20 30.12e-20")
    assert(result sameElements Array("2.5e-2", "30.12e-2", "2.5e-20", "30.12e-20"))
  }

  test("numbers: scientific with float mantissa, no-sign in exponent, big E") {
    val result = Tokenizer.tokenize("2.5E2 30.12E2 2.5E20 30.12E20")
    assert(result sameElements Array("2.5E2", "30.12E2", "2.5E20", "30.12E20"))
  }

  test("numbers: scientific with float mantissa, plus in exponent, big E") {
    val result = Tokenizer.tokenize("2.5E+2 30.12E+2 2.5E+20 30.12E+20")
    assert(result sameElements Array("2.5E+2", "30.12E+2", "2.5E+20", "30.12E+20"))
  }

  test("numbers: scientific with float mantissa, minus in exponent, big E") {
    val result = Tokenizer.tokenize("2.5E-2 30.12E-2 2.5E-20 30.12E-20")
    assert(result sameElements Array("2.5E-2", "30.12E-2", "2.5E-20", "30.12E-20"))
  }


  test("other letters in place of E are interpreted as symbols") {
    assert(Tokenizer.tokenize("3x2") sameElements Array("3", "x2"))
    assert(Tokenizer.tokenize("3m-2") sameElements Array("3", "m", "-", "2"))
    assert(Tokenizer.tokenize("30s+20") sameElements Array("30", "s", "+", "20"))
  }

  test("space after e means e is symbol") {
    assert(Tokenizer.tokenize("2.5e 2") sameElements Array("2.5", "e", "2"))
    assert(Tokenizer.tokenize("2.5e -2") sameElements Array("2.5", "e", "-", "2"))
    assert(Tokenizer.tokenize("2.5e +2") sameElements Array("2.5", "e", "+", "2"))
  }

  // END NUMBERS



  // SYMBOLS

  test("any group of letters is a symbol") {
    assert(Tokenizer.tokenize("s kg KiB µ µm Å Ω µΩ öö 猫 猫猫")
      sameElements Array("s", "kg", "KiB", "µ", "µm", "Å", "Ω", "µΩ", "öö", "猫", "猫猫"))
  }

  test("underscore is valid character in symbols") {
    assert(Tokenizer.tokenize("a_b _a _bc xxx_ _1 _1d ")
      sameElements Array("a_b", "_a", "_bc", "xxx_", "_1", "_1d"))
  }

  test("currency symbols are valid characters in symbols") {
    assert(Tokenizer.tokenize("$ € £ ¤") sameElements Array("$", "€", "£", "¤"))
  }

  test("apostrophe is valid character in symbols") {
    assert(Tokenizer.tokenize("35'") sameElements Array("35", "'"))
  }

  test("percent, permille, and degree are valid symbol characters") {
    assert(Tokenizer.tokenize("10% + 120‰ · 45°") sameElements Array("10", "%", "+", "120", "‰", "·", "45", "°"))
  }

  test("symbols can have numbers except as first letter") {
    assert(Tokenizer.tokenize("a1 bob2 c20 d5e6 2f 3.5€")
      sameElements Array("a1", "bob2", "c20", "d5e6", "2", "f", "3.5", "€"))
  }

  // END SYMBOLS


  // SPACES

  test("spaces are ignored at border of tokens") {
    val result = Tokenizer.tokenize("1 + 2 * 3   /   4 - 5 ^ 6")
    assert(result sameElements Array("1", "+", "2", "*", "3", "/", "4", "-", "5", "^", "6"))
  }

  test("spaces separate tokens") {
    val result = Tokenizer.tokenize("2 m m/(4 s s)")
    assert(result sameElements Array("2", "m", "m", "/", "(", "4", "s", "s", ")"))
  }

  test("leading and trailing white-space is ignored") {
    val result = Tokenizer.tokenize(" 2 + 3 ")
    assert(result sameElements Array("2", "+", "3"))
  }

  // END SPACES



  test("parenthezes") {
    val result = Tokenizer.tokenize("1+2*(3-4)/((2))")
    assert(result sameElements Array("1", "+", "2", "*", "(", "3", "-", "4", ")", "/", "(", "(", "2", ")", ")"))
  }



  // OPERATORS

  test("binary operators") {
    val result = Tokenizer.tokenize("1+2*3/4-5^6·7:8,9;0=1?2")
    assert(result sameElements Array(
      "1", "+", "2", "*", "3", "/", "4", "-", "5", "^", "6", "·", "7", ":", "8", ",", "9", ";", "0", "=", "1", "?", "2"
    ))
  }

  test("function operators") {
    assert(Tokenizer.tokenize("sqrt(1)") sameElements Array("sqrt", "(", "1", ")"))
    assert(Tokenizer.tokenize("log(4, 2)") sameElements Array("log", "(", "4", ",", "2", ")"))
  }

  test("unary operators") {
    val result = Tokenizer.tokenize("+++1 + ---2")
    assert(result sameElements Array("+", "+", "+", "1", "+", "-", "-", "-", "2"))
  }

  test("expressions with symbols") {
    assert(Tokenizer.tokenize("2m+3m") sameElements Array("2", "m", "+", "3", "m"))
    assert(Tokenizer.tokenize("6m^3/2m^2") sameElements Array("6", "m", "^", "3", "/", "2", "m", "^", "2"))
  }

  test("implicit multiplication of parenthetical by number 1") {
    val result = Tokenizer.tokenize("2 (3 + 4)")
    assert(result sameElements Array("2", "(", "3", "+", "4", ")"))
  }

  test("implicit multiplication of parenthetical by number 2") {
    val result = Tokenizer.tokenize("2(3 + 4)")
    assert(result sameElements Array("2", "(", "3", "+", "4", ")"))
  }

  test("implicit multiplication of parenthetical by symbol") {
    val result = Tokenizer.tokenize("x(3 + 4)")
    assert(result sameElements Array("x", "(", "3", "+", "4", ")"))
  }

  test("implicit multiplication of parenthetical by symbol, reverse") {
    val result = Tokenizer.tokenize("(3 + 4)x")
    assert(result sameElements Array("(", "3", "+", "4", ")", "x"))
  }

  test("implicit multiplication of parenthetical by parenthetical") {
    val result = Tokenizer.tokenize("(1 + 2)(3 + 4)")
    assert(result sameElements Array("(", "1", "+", "2", ")", "(", "3", "+", "4", ")"))
  }

  test("implicit multiplication of symbol 1") {
    val result = Tokenizer.tokenize("2 m")
    assert(result sameElements Array("2", "m"))
  }

  test("implicit multiplication of symbol 2") {
    val result = Tokenizer.tokenize("2m")
    assert(result sameElements Array("2", "m"))
  }

  // END OPERATORS



  // STRINGS

  test("strings in expressions") {
    val result = Tokenizer.tokenize("""   2 "metre" / 4 "second"   """.trim)
    assert(result sameElements Array("2", q("metre"), "/", "4", q("second")))
  }

  test("multiword-strings in expressions") {
    val result = Tokenizer.tokenize("""   atm = "technical atmosphere"   """.trim)
    assert(result sameElements Array("atm", "=", q("technical atmosphere")))
  }

  test("other tokens in strings are ignored") {
    val result = Tokenizer.tokenize(""" "1 + 2 cos(x^2)" """.trim)
    assert(result sameElements Array(q("1 + 2 cos(x^2)")))
  }

  // END STRINGS



  // SYNTAX ERRORS

  test("throws on dangling operator") {
    intercept[java.lang.Error] {
      Tokenizer.tokenize("2 + 3 -")
    }
  }

  test("throws on mismatched parenthesis 1") {
    intercept[java.lang.Error] {
      Tokenizer.tokenize("2 + (3 - 4")
    }
  }

  test("throws on mismatched parenthesis 2") {
    intercept[java.lang.Error] {
      Tokenizer.tokenize("2 + 3) - 4")
    }
  }

  test("throws on unknown punctuation") {
    intercept[java.lang.Error] {
      Tokenizer.tokenize("2 & 3")
    }
    intercept[java.lang.Error] {
      Tokenizer.tokenize("2 'metre'")
    }
  }

}
