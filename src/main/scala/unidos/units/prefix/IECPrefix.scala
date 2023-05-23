package unidos.units.prefix


object IECPrefix extends Prefix {
  override val byText = Map[String, Double](
    "yobi" -> Math.pow(2, 80),
    "zebi" -> Math.pow(2, 70),
    "exbi" -> Math.pow(2, 60),
    "pebi" -> Math.pow(2, 50),
    "tebi" -> Math.pow(2, 40),
    "gibi" -> Math.pow(2, 30),
    "mebi" -> Math.pow(2, 20),
    "kibi" -> Math.pow(2, 10),
    "" -> 1,
  )

}
