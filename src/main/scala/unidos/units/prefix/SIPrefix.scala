package unidos.units.prefix


object SIPrefix extends Prefix {
  override val byText = Map[String, Double](
    "quetta" -> 1e30,
    "ronna" -> 1e27,
    "yotta" -> 1e24,
    "zetta" -> 1e21,
    "exa" -> 1e18,
    "peta" -> 1e15,
    "tera" -> 1e12,
    "giga" -> 1e9,
    "mega" -> 1e6,
    "kilo" -> 1e3,
    "hecto" -> 1e2,
    "deca" -> 1e1,
    "" -> 1,
    "deci" -> 1e-1,
    "centi" -> 1e-2,
    "milli" -> 1e-3,
    "micro" -> 1e-6,
    "nano" -> 1e-9,
    "pico" -> 1e-12,
    "femto" -> 1e-15,
    "atto" -> 1e-18,
    "zepto" -> 1e-21,
    "yocto" -> 1e-24,
    "ronto" -> 1e-27,
    "quecto" -> 1e-30,
  )
}
