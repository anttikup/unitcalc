package unidos.shuntingyard.tokenizer

import scala.collection.mutable.Stack

class Cursor(val expr: String, var pos: Int) {
  val output = Stack[String]()
  val startPos = Stack[Int]()
  val startNo = Stack[Int]()

  def current: Char =
    expr(pos)

  def startSubtoken(desc: String = "") = {
    startPos.push(pos)
  }

  def startToken(desc: String = "") = {
    startNo.push(output.length)
    startSubtoken(desc)
  }

  def rollback(): Boolean = {
    val no = startNo.pop()
    val startedAt = startPos.pop()
    output.dropRight(output.length - no)
    pos = startedAt
    false
  }

  def reset(): Boolean = {
    pos = startPos.pop()
    false
  }

  def confirm(): Boolean = {
    startPos.pop()
    true
  }

  def commit(): Boolean = {
    val no = startNo.pop()
    val startedAt = startPos.pop()
    val token = expr.substring(startedAt, pos)
    output.push(token)
    true
  }

  def hasToken =
    startPos.length > 0 && startPos.top < pos

  def token =
    expr.substring(startPos.top, pos)

  def rest =
    expr.substring(pos)

  def atEnd =
    (pos == expr.length - 1)
}
