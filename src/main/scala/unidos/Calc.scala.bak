package unidos

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Stack, Queue}

import unidos.shuntingyard.Parser
import unidos.units.{Unido, Unitful}


object Env {
  val defs = new HashMap[String, Unido]()

  def `import`(name: String, as: String): Unido = {
    Unido.get(name) match {
      case Some(unit) => {
        println(s"$name -> $as")
        defs.put(as, unit)
        unit
      }
      case None => {
        throw new Exception(s"No such unit $name")
      }
    }
  }

  def get(short: String): Option[Unido] = defs.get(short)
}

object Calc {
  val operators = Set[String]("+", "-", "*", "/", "^", "=", "·")

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
  }

  def calc(expr: String): Unitful = {
    val rpn = Parser.parse(expr)
    val varStack = Stack[Unitful]()

    for ( token <- rpn ) {
      println(s"Handling: $token")
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
        case "^" => {
          val right = varStack.pop
          val left = varStack.pop
          varStack.push(left ^ right)
        }
        case variable => {
          Env.get(variable) match {
            case Some(unit) => {
              varStack.push(new Unitful(1, unit))
            }
            case None => {
              varStack.push(new Unitful(variable.toDouble))
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
      case pattern(short, full) => {
        println(s"$short: $full")
        val unit = Env.`import`(full, short)
        new Unitful(1, unit)
      }
      case _ => calc(expr)
    }
  }
}
