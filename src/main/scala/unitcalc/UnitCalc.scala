package unitcalc

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.DomApi
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom

import unidos.Calc
import unidos.units.List
import unitcalc.Help


@main
def UnitCalc(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )

object Main {
  val model = new Model
  import model.*

  List.load
  Calc.preload

  def appElement(): Element =
    div(
      className := "main",
      h1("Unit calculator"),
      div(
        className := "scroll-pane",
        renderExprTable(),
      ),
      div(
        className := "error",
        child <-- errorSignal.map(msg => msg)
      ),
      div(
        className := "bottom",
        foreignHtmlElement(DomApi.unsafeParseHtmlString(Help.text))
      )
    )
  end appElement

  def renderExprTable(): Element =
    table(
      thead(tr(th("Expression"), th("Unit"), th("Quantity"))),
      tbody(
        children <-- exprSignal.split(_.id) { (id, initial, itemSignal) =>
          renderExprItem(id, itemSignal)
        }
      ),
      tfoot(
        tr(
          td(
            input(
              typ := "text",
              idAttr := "expr-input",
              value <-- inputSignal.map(str => str.replace("*", "·").replace("-", "−")),
              onInput.mapToValue --> inputVar,
              onKeyPress --> (event => event.key match { case "Enter" => calculate() case _ => ; })
            )
          ),
          td(button("🟰", onClick --> (_ => calculate() ))),
          td(
            child.text <-- inputSignal.map(item => "")
          ),
        )
      )
    )
  end renderExprTable

  def renderExprItem(id: ExprItemID, itemSignal: Signal[ExprItem]): Element = {
    tr(
      td(
        child.text <-- itemSignal.map(item => item.label)
      ),
      td(
        child.text <-- itemSignal.map(item => item.unit)
      ),
      td(
        child.text <-- itemSignal.map(item => item.dims)
      ),
    )
  }


  def calculate() = {

    val expr = getInputValue

    val exprs = expr.split(";")
    exprs.foreach(expr => {
      try {
        val result = Calc.eval(expr)
        val resultVal = result.amount
        val resultUnit = result.unit
        val unitStr = resultUnit.name
        val dimsStr = resultUnit.quantity.name

        addExprItem(ExprItem(expr))
        addExprItem(ExprItem(s"$resultVal", s"$unitStr", dimsStr))
        inputVar.update(item => "")
        errorVar.update(item => "")
      } catch {
        case err => errorVar.update(item => err.toString)
      }
    })

    //js.eval("document.getElementById('expr-input').focus();")
    dom.document.getElementById("expr-input").asInstanceOf[dom.HTMLElement].focus()
  }

}
