package unidos.units


case class NamePart(text: String, value: Int) {
  override def toString: String = {
    text + {
      if (value != 1)
        Util.getPower(value)
      else
        ""
    }
  }

  def invert: NamePart =
    NamePart(text, -value)

  def asTuple: (String, Int) =
    (text, value)
}

case class CompoundName(args: (String, Int)*) {
  val parts = args.map(arg => NamePart(arg._1, arg._2))
                  .sortWith(
                     (a, b) =>
                       if (a.value == b.value)
                         a.text > b.text
                       else
                         a.value < b.value
                  )
                  .reverse


  override def toString: String = {
    if ( parts(0).value < 0 )
      "1" + parts.map(
        part =>
          if (part.value < 0)
            s"/" + part.invert.toString
          else
            " " + part.toString
      ).mkString("")
    else
      parts(0).toString + parts.tail.map(
        part =>
          if (part.value < 0)
            s"/" + part.invert.toString
          else
            " " + part.toString
      ).mkString("")
  }

  def invert: CompoundName =
    CompoundName(this.parts.map(part => part.invert.asTuple):_*)

  def *(other: CompoundName): CompoundName = {
    CompoundName((args ++ other.args):_*)
  }

  def /(other: CompoundName): CompoundName = {
    CompoundName((args ++ other.invert.args):_*)
  }

}
