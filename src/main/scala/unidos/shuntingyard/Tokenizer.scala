package unidos.shuntingyard

object Tokenizer {
  def tokenize(expr: String): Array[String] = {
    var regex = raw"([0-9]*\.[0-9]+[eE][-]?[0-9]+|[0-9]+[eE][-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+|[-−+/*·()^]|[^-−^+/*· )(]*| *)".r

    for ( token <- regex.findAllIn(expr).toArray if token.strip != "" ) yield token
  }
}
