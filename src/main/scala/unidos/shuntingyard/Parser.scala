package unidos.shuntingyard

import scala.collection.mutable.{Stack, Queue, SortedSet}


object Exp extends Enumeration {
  type Exp = Value
  val Operator, Operand = Value
}

object Parser {
  val precedence = Map[String, Int](
    "+" -> 5,
    "-" -> 5,
    "*" -> 10,
    "·" -> 10,
    "/" -> 10,
    "•" -> 12,
    "^" -> 15,
    "(" -> 0
  )
  val unary = Set[String]("+", "-")
  val binary = Set[String]("+", "-", "*", "/", "^", "•", "·")


  def parse(expr: String): Queue[String] = {
    val tokens = Tokenizer.tokenize(expr)
    val stack = new Stack[String]()
    val output = new Queue[String]()

    var exp = Exp.Operand
    var prev = Exp.Operator
    for ( token <- tokens ) {
      println(s"$exp -> token: $token; stack ${stack.length}")
      if ( binary(token) && exp == Exp.Operator ) {
        val tokenPrecedence = precedence.get(token).get

        while (!stack.isEmpty && precedence.get(stack.top).get >= tokenPrecedence) {
          output += stack.pop()
        }

        stack.push(token)
        exp = Exp.Operand
        prev = Exp.Operator
      } else if ( unary(token) && exp == Exp.Operand) {
        stack.push(token)
        exp = Exp.Operand
        prev = Exp.Operator
      } else if ( token == "(" && exp == Exp.Operand) {
        stack.push(token)
        exp = Exp.Operand
        prev = Exp.Operator
      } else if ( token == ")" && exp == Exp.Operator) {
        while (!stack.isEmpty && stack.top != "(") {
          output += stack.pop
        }
        if ( !stack.isEmpty )
          stack.pop
        exp = Exp.Operator
        prev = Exp.Operand
      } else if ( !unary(token) && !binary(token) ) {
        if ( prev == Exp.Operand )
          stack.push("·")
        output += token
        exp = Exp.Operator
        prev = Exp.Operand
      } else {
        throw new Error(s"Unexpected token: $token, $exp")
      }
    }

    while (stack.length > 0 ) {
      output += stack.pop
    }

    return output
  }
}
