package unitcalc

import unitcalc.{ExprItem, ExprItemID}

class ModelTest extends munit.FunSuite {
  test("label") {
    val item = ExprItem(ExprItemID(), "test")
    assert(item.label == "test")
  }

  test("addExprItem") {
    val model = new Model

    val item = ExprItem(ExprItemID(), "test")
    model.addExprItem(item)

    val afterItems = model.exprSignal.now()

    assert(afterItems.size == 1)
    assert(afterItems.last == item)
  }

  test("removeExprItem") {
    val model = new Model

    model.addExprItem(ExprItem(ExprItemID(), "test"))

    val beforeItems = model.exprSignal.now()
    assert(beforeItems.size == 1)

    model.removeExprItem(beforeItems.head.id)

    val afterItems = model.exprSignal.now()
    assert(afterItems.size == 0)
    assert(afterItems == beforeItems.tail)
  }

}
