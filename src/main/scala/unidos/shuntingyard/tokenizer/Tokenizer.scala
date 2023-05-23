package unidos.shuntingyard.tokenizer

object Tokenizer {

  // Valid characters in symbols besides letters and numbers
  val otherValidSymbolChars = "%‰'°_$€£¤"
  val binaryOperators = "+-·*^/:=,;?"
  val unaryOperators = "+-"

  def tokenize(expr: String): Array[String] = {
    val cursor = new Cursor(expr.trim + "\u0000", 0)
    readExpression(cursor)
    if ( !cursor.atEnd )
      throw new Error(s"Unexpected token: ${cursor.rest}'")
    cursor.output.reverse.toArray
  }

  def readExpression(cursor: Cursor): Boolean = {
    if (readValueLike(cursor)) {
      readSpace(cursor)
      readOperator(cursor) && readSpace(cursor) && readExpression(cursor)
      true
    } else {
      false
    }
  }

  def readValueLike(cursor: Cursor): Boolean = {
    cursor.startSubtoken("value-like")
    if (readValue(cursor) || readParenthetical(cursor) || readUnaryExpression(cursor))
      cursor.confirm()
    else
      cursor.reset()
  }

  def readSpace(cursor: Cursor): Boolean = {
    cursor.startSubtoken("space")
    while (" ".contains(cursor.current))
      cursor.pos += 1

    cursor.confirm()
  }

  def readOperator(cursor: Cursor): Boolean =
    readExplicitOperator(cursor) || readImplicitMultiplication(cursor)


  def readValue(cursor: Cursor): Boolean =
    readNumber(cursor) || readSymbol(cursor) || readString(cursor)

  def readParenthetical(cursor: Cursor): Boolean =
    readChar(cursor, '(') && readExpression(cursor) && readChar(cursor, ')')

  def readUnaryExpression(cursor: Cursor): Boolean = {
    cursor.startSubtoken("unary expr")
    if (readUnaryOperator(cursor) && readSpace(cursor) && readValueLike(cursor))
      cursor.confirm()
    else
      cursor.reset()
  }

  def readExplicitOperator(cursor: Cursor): Boolean = {
    cursor.startToken("expl-op")
    advanceChar(cursor, binaryOperators)

    if (cursor.hasToken)
      cursor.commit()
    else
      cursor.rollback()
  }

  def readImplicitMultiplication(cursor: Cursor): Boolean =
    readSpace(cursor)


  def readNumber(cursor: Cursor): Boolean = {
    cursor.startToken("number")
    if (advanceScientific(cursor) || advanceFloat(cursor) || advanceInteger(cursor))
      cursor.commit()
    else
      cursor.rollback()
  }

  def readSymbol(cursor: Cursor): Boolean = {
    cursor.startToken("var")

    // Numbers aren't allowed as first letters, so that it's possible to write, eg. 2m.
    if ( cursor.current.isLetter || otherValidSymbolChars.contains(cursor.current) ) {
      cursor.pos += 1

      while (cursor.current.isLetterOrDigit || otherValidSymbolChars.contains(cursor.current))
        cursor.pos += 1
    }

    if (cursor.hasToken)
      cursor.commit()
    else
      cursor.rollback()
  }

  def readString(cursor: Cursor): Boolean = {
    cursor.startToken("str")
    if (advanceString(cursor))
      cursor.commit()
    else
      cursor.rollback()
  }

  def readUnaryOperator(cursor: Cursor): Boolean = {
    cursor.startToken("unary operator")
    if (advanceChar(cursor, unaryOperators))
      cursor.commit()
    else
      cursor.rollback()
  }


  def readChar(cursor: Cursor, char: Char): Boolean = {
    cursor.startToken()
    if (advanceChar(cursor, char))
      cursor.commit()
    else
      cursor.rollback()
  }

  def advanceScientific(cursor: Cursor): Boolean = {
    cursor.startSubtoken("sci")
    if (
      (advanceFloat(cursor) || advanceInteger(cursor))
        && advanceChar(cursor, "eE")
        && optional(advanceChar(cursor, "+-"))
        && advanceInteger(cursor)
    )
      cursor.confirm()
    else
      cursor.reset()
  }

  def advanceFloat(cursor: Cursor): Boolean = {
    cursor.startSubtoken("float")
    if ( optional(advanceInteger(cursor)) && advanceChar(cursor, '.') && advanceInteger(cursor) )
      cursor.confirm()
    else
      cursor.reset()
  }

  def advanceInteger(cursor: Cursor): Boolean = {
    cursor.startSubtoken("integer")
    while (cursor.current.isDigit)
      cursor.pos += 1

    if (cursor.hasToken)
      cursor.confirm()
    else
      cursor.reset()
  }

  def advanceChar(cursor: Cursor, char: Char): Boolean = {
    if (cursor.current == char) {
      cursor.pos += 1
      true
    } else {
      false
    }
  }

  def advanceChar(cursor: Cursor, acceptableChars: String): Boolean = {
    if (acceptableChars.contains(cursor.current)) {
      cursor.pos += 1
      true
    } else {
      false
    }
  }

  def optional(result: Boolean): Boolean =
    true

  def advanceUntilChar(cursor: Cursor, char: Char): Boolean = {
    while (cursor.current != char && !cursor.atEnd)
      cursor.pos += 1

    if (cursor.current == char)
      true
    else
      false
  }

  def advanceString(cursor: Cursor): Boolean = {
    cursor.startSubtoken("str")

    if (advanceChar(cursor, '"') && advanceUntilChar(cursor, '"') && advanceChar(cursor, '"'))
      cursor.confirm()
    else
      cursor.reset()
  }


}
