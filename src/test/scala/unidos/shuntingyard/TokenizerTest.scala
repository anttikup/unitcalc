package unidos

import unidos.shuntingyard.Tokenizer


class TokenizerTest extends munit.FunSuite {
  def printArray(arr: Array[String]): Unit = {
    for ( item <- arr ) {
      print(s"\"$item\" ")
    }
    println("")
  }
  test("numbers") {
    val result = Tokenizer.tokenize("5 879 2.2 2.878 .5 .802 1.2e3 2.183e-6 1.2e18 2.186e-21")
    assert(result sameElements Array("5", "879", "2.2", "2.878", ".5", ".802", "1.2e3", "2.183e-6", "1.2e18", "2.186e-21"))
  }

  test("basic operators") {
    val result = Tokenizer.tokenize("1+2*3/4-5^6·7")
    assert(result sameElements Array("1", "+", "2", "*", "3", "/", "4", "-", "5", "^", "6", "·", "7"))
  }

  test("spaces are ignored at border of tokens") {
    val result = Tokenizer.tokenize("1 + 2 * 3   /   4 - 5 ^ 6")
    assert(result sameElements Array("1", "+", "2", "*", "3", "/", "4", "-", "5", "^", "6"))
  }

  test("spaces separate tokens") {
    val result = Tokenizer.tokenize("2 m m/(4 s s)")
    assert(result sameElements Array("2", "m", "m", "/", "(", "4", "s", "s", ")"))
  }


  test("parenthezes") {
    val result = Tokenizer.tokenize("1+2*(3-4)/((2))")
    assert(result sameElements Array("1", "+", "2", "*", "(", "3", "-", "4", ")", "/", "(", "(", "2", ")", ")"))
  }

  test("function operators") {
    assert(Tokenizer.tokenize("sqrt(1)") sameElements Array("sqrt", "(", "1", ")"))
    assert(Tokenizer.tokenize("log(4, 2)") sameElements Array("log", "(", "4", ",", "2", ")"))
  }

  test("expressions with units") {
    assert(Tokenizer.tokenize("2m+3m") sameElements Array("2", "m", "+", "3", "m"))
    assert(Tokenizer.tokenize("6m^3/2m^2") sameElements Array("6", "m", "^", "3", "/", "2", "m", "^", "2"))
  }

}
