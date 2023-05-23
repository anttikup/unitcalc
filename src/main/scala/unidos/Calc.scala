package unidos

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Stack, Queue}

import unidos.shuntingyard.Parser
import unidos.units.{Unido, Unidos, Unitful}


object Env {
  val defs = new HashMap[String, Unitful]()

  def setVariable(symbol: String, value: Unitful): Unitful = {
    defs.put(symbol, value)
    value
  }

  def setSymbol(symbol: String, name: String): Unitful = {
    Unidos.get(name) match {
      case Some(unit) => setVariable(symbol, Unitful(1, unit))
      case None => throw new Error(s"No such unit: $name")
    }
  }

  def get(symbol: String): Option[Unitful] =
    defs.get(symbol)

}

object Calc {
  val operators = Set[String]("+", "-", "−", "*", "·", "/", "^", "=")

  def preload: Unit = {
    Env.setSymbol("s", "second")
    Env.setSymbol("m", "metre")
    Env.setSymbol("kg", "kilogram")
    Env.setSymbol("A", "ampere")
    Env.setSymbol("K", "kelvin")
    Env.setSymbol("mol", "mole")
    Env.setSymbol("cd", "candela")
    Env.setSymbol("rad", "radian")
    Env.setSymbol("sr", "steradian")
    Env.setSymbol("Hz", "hertz")
    Env.setSymbol("N", "newton")
    Env.setSymbol("Pa", "pascal")
    Env.setSymbol("J", "joule")
    Env.setSymbol("W", "watt")
    Env.setSymbol("V", "volt")
    Env.setSymbol("C", "coulomb")
    Env.setSymbol("`Ω`", "ohm")
    Env.setSymbol("S", "siemens")
    Env.setSymbol("Wb", "weber")
    Env.setSymbol("T", "tesla")
    Env.setSymbol("H", "henry")
    Env.setSymbol("lm", "lumen")
    Env.setSymbol("lx", "lux")
    Env.setSymbol("Bq", "becquerel")
    Env.setSymbol("Gy", "gray")
    Env.setSymbol("Sv", "sievert")
    Env.setSymbol("kat", "katal")

    Env.setSymbol("min", "minute")
    Env.setSymbol("h", "hour")

    Env.setSymbol("km", "kilometre")
    Env.setSymbol("g", "gram")

    Env.setSymbol("¤", "unit of money")
    Env.setSymbol("B", "byte")
  }

  def calc(expr: String): Unitful = {
    val rpn = Parser.parse(expr)
    val varStack = Stack[Unitful]()

    for ( token <- rpn ) {
      token match {
        case "*" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left * right)
        }
        case "·" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left * right)
        }
        case "/" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left / right)
        }
        case "+" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left + right)
        }
        case "-" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left - right)
        }
        case "−" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left - right)
        }
        case "^" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(Unitful.pow(left, right))
        }
        case "sqrt" => {
          val param = varStack.pop
          varStack.push(Unitful.root(param, 2))
        }
        case "cbrt" => {
          val param = varStack.pop
          varStack.push(Unitful.root(param, 3))
        }
        case "?" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left ? right)
        }
        case variable => {
          Env.get(variable) match {
            case Some(value) => {
              varStack.push(value)
            }
            case None => {
              if (variable.startsWith("\"") && variable.endsWith("\"")) {
                val unitName = variable.substring(1, variable.length - 1)
                Unidos.get(unitName) match {
                  case Some(unit) => {
                    val newValue = new Unitful(1, unit)
                    varStack.push(newValue)
                  }
                  case None => throw new Error(s"No such unit: $unitName")
                }
              } else {
                val d = variable.toDouble
                val newValue = new Unitful(d)
                varStack.push(newValue)
              }
            }
          }
        }
      }
    }

    return varStack.pop
  }

  def prepareExpression(expr: String) =
    if (expr(0) == '?')
      '_' + expr
    else
      expr

  def eval(exprIn: String): Unitful = {
    val expr = prepareExpression(exprIn)
    val pattern = "^ *([^ =]+) *= *(.+)$".r
    expr match {
      case pattern(symbol, expr) => {
        Env.setVariable(symbol, calc(expr))
      }
      case _ => Env.setVariable("_", calc(expr))
    }
  }
}
