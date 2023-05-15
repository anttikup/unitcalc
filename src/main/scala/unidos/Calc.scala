package unidos

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Stack, Queue}

import unidos.shuntingyard.Parser
import unidos.units.{Unido, Unidos, Unitful}


object Env {
  val defs = new HashMap[String, Unido]()

  def `import`(name: String, symbol: String): Unido = {
    Unidos.get(name) match {
      case Some(unit) => {
        println(s"Import: $name -> $symbol")
        defs.put(symbol, unit)
        unit
      }
      case None => {
        throw new Exception(s"No such unit $name")
      }
    }
  }

  def get(symbol: String): Option[Unido] =
    defs.get(symbol)
}

object Calc {
  val operators = Set[String]("+", "-", "−", "*", "·", "/", "^", "=")

  def preload: Unit = {
    Env.`import`("second", "s")
    Env.`import`("metre", "m")
    Env.`import`("kilogram", "kg")
    Env.`import`("ampere", "A")
    Env.`import`("kelvin", "K")
    Env.`import`("mole", "mol")
    Env.`import`("candela", "cd")
    Env.`import`("radian", "rad")
    Env.`import`("steradian", "sr")
    Env.`import`("hertz", "Hz")
    Env.`import`("newton", "N")
    Env.`import`("pascal", "Pa")
    Env.`import`("joule", "J")
    Env.`import`("watt", "W")
    Env.`import`("volt", "V")
    Env.`import`("coulomb", "C")
    Env.`import`("ohm", "`Ω`")
    Env.`import`("siemens", "S")
    Env.`import`("weber", "Wb")
    Env.`import`("tesla", "T")
    Env.`import`("henry", "H")
    Env.`import`("lumen", "lm")
    Env.`import`("lux", "lx")
    Env.`import`("becquerel", "Bq")
    Env.`import`("gray", "Gy")
    Env.`import`("sievert", "Sv")
    Env.`import`("katal", "kat")

    Env.`import`("minute", "min")
    Env.`import`("hour", "h")

    Env.`import`("kilometre", "km")
    Env.`import`("gram", "g")
  }

  def calc(expr: String): Unitful = {
    val rpn = Parser.parse(expr)
    val varStack = Stack[Unitful]()

    for ( token <- rpn ) {
      //println(s"Handling: $token")
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
            case Some(unit) => {
              varStack.push(new Unitful(1, unit))
            }
            case None => {
              val d = variable.toDouble
              val newValue = new Unitful(d)
              varStack.push(newValue)
            }
          }
        }
      }
    }

    return varStack.pop
  }

  def eval(expr: String): Unitful = {
    val pattern = "^ *([^ =]+) *: *([^ ]+)$".r
    expr match {
      case pattern(symbol, full) => {
        val unit = Env.`import`(full, symbol)
        new Unitful(1, unit)
      }
      case _ => calc(expr)
    }
  }
}
