package unitcalc

import scala.util.Random
import com.raquo.laminar.api.L.{*, given}

final class ExprItemID

case class ExprItem(id: ExprItemID, label: String, unit: String = "", dims: String = ""):
  def fullPrice: Double = 5

object ExprItem {
  def apply(): ExprItem =
    ExprItem(ExprItemID(), "")

  def apply(label: String): ExprItem =
    ExprItem(ExprItemID(), label)

  def apply(label: String, unit: String, dims: String): ExprItem =
    ExprItem(ExprItemID(), label, unit, dims)

}

type ExprList = List[ExprItem]


final class Model {
  val exprVar: Var[ExprList] = Var(List())
  val exprSignal = exprVar.signal
  val inputVar: Var[String] = Var("")
  val inputSignal = inputVar.signal
  val errorVar: Var[String] = Var("")
  val errorSignal = errorVar.signal

  def addExprItem(item: ExprItem): Unit =
    exprVar.update(expr => expr :+ item)

  def removeExprItem(id: ExprItemID): Unit =
    exprVar.update(expr => expr.filter(_.id != id))

  def getExprItemValue(id: ExprItemID): Option[ExprItem] =
    exprSignal.now().find(item => item.id == id)

  def isLastItem(id: ExprItemID): Boolean =
    exprSignal.now().last.id == id

  def lastID: ExprItemID =
    exprSignal.now().last.id

  def getInputValue: String =
    inputSignal.now()

}
