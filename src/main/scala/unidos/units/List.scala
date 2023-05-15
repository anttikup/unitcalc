package unidos.units

import scala.math.{pow, Pi}

import unidos.units.prefix.SIPrefix


object List {
  def load = {
    val Array(dimensionless, time, length, mass, current, temp, amount, intensity) =
      Quantity.createBaseQuantities(
        Array(
          "dimensionless",
          "time",
          "length",
          "mass",
          "electric current",
          "temperature",
          "amount of substance",
          "luminous intensity"
        )
      )

    // Quantities
    val freq = Quantity.create("frequency", dimensionless/time)
    val area = Quantity.create("area", length*length)
    val volume = Quantity.create("volume", area*length)
    val speed = Quantity.create("speed", length/time)
    val force = Quantity.create("force", length*mass/(time*time))
    val pressure = Quantity.create("pressure", mass/length/time/time)
    val acceleration = Quantity.create("acceleration", speed/time)
    val planeAngle = Quantity.create("plane angle", dimensionless)
    val solidAngle = Quantity.create("solid angle", dimensionless)

    val energy = Quantity.create("energy", force*length)
    val power = Quantity.create("power", energy/time)
    val charge = Quantity.create("electric charge", current*time)
    val voltage = Quantity.create("voltage", power/current)
    val capacitance = Quantity.create("capacitance", charge/voltage)
    val resistance = Quantity.create("resistance", voltage/current)
    val conductance = Quantity.create("conductance", dimensionless/resistance)
    val magneticFlux = Quantity.create("magnetic flux", voltage*time)
    val magneticFluxDensity = Quantity.create("magnetic flux density", magneticFlux/length/length)
    val inductance = Quantity.create("inductance", magneticFlux/current)
    val lightTodo = Quantity.create("light todo", intensity*solidAngle)
    val lightTodo2 = Quantity.create("light todo 2", lightTodo/length/length)
    val activity = Quantity.create("activity", dimensionless/time)
    val absorbedTodo = Quantity.create("absorbed todo", energy/mass)
    val equivalentTodo = Quantity.create("equivalent todo", energy/mass)
    val catalyticActivity = Quantity.create("catalytic activity", amount/time)

    // Base units
    val `1` = Unido.create("1", 1, dimensionless)
    val s = Unido.create("second", 1, time)

    SIPrefix.createUnits("metre", length)
    val m = Unido("metre")

    SIPrefix.createUnits("gram", mass, 1e-3)
    val kg = Unido("kilogram")

    SIPrefix.createUnits("ampere", current)
    val A = Unido("ampere")

    SIPrefix.createUnits("kelvin", temp)
    val K = Unido("kelvin")

    SIPrefix.createUnits("mole", amount)
    val mol = Unido("mole")

    SIPrefix.createUnits("candela", intensity)
    val cd = Unido("candela")


    // Derived units
    val rad = Unido.create("radian", 1, planeAngle)
    val sr = Unido.create("steradian", 1, solidAngle)
    val Hz = Unido.create("hertz", 1, freq)
    val N = Unido.create("newton", 1, force)
    val Pa = Unido.create("pascal", 1, pressure)
    val J = Unido.create("joule", N * m)
    val W = Unido.create("watt", J / s)
    val V = Unido.create("volt", W / A)
    val C = Unido.create("coulomb", A * s)
    val `Ω` = Unido.create("ohm", V / A)
    val S = Unido.create("siemens", `1`/`Ω`)
    val Wb = Unido.create("weber", V*s)
    val T = Unido.create("tesla", Wb/m/m)
    val H = Unido.create("henry", Wb/A)
    val lm = Unido.create("lumen", cd*sr)
    val lx = Unido.create("lux", lm/m/m)
    val Bq = Unido.create("becquerel", 1, activity)
    val Gy = Unido.create("gray", J/kg)
    val Sv = Unido.create("sievert", J/kg)
    val kat = Unido.create("katal", mol/s)

    //val `m/s` = Unido.create("meters per second", m / s)


    val dm = Unido.create("decimetre", m / 10)
    val kPa = Unido.create("kilopascal", Pa * 1000)

    val `dm³` = Unido.create("decimetre³", Unido.pow(dm, 3))
    val L = Unido.create("litre", `dm³`)

    // Other units
    val min = Unido.create("minute", s * 60)
    val h = Unido.create("hour", min * 60)
    val `°` = Unido.create("degree", rad * (2*Pi/360))
    val `′` = Unido.create("arcminute", `°` / 60)
    val `″` = Unido.create("arcsecond", `′` / 60)
    val gon = Unido.create("gradian", rad * (2*Pi/400))
    val AU = Unido.create("astronomical unit", m * 149597.9e6)
    val pc = Unido.create("parsek", m * 3.085678e16)
    val eV = Unido.create("electron volt", J * 1.6021773e-19)
    val u = Unido.create("atomic mass unit", kg * 1.6605402e-27)
    val bar = Unido.create("bar", Pa * 1e5)
    val `Å` = Unido.create("ångström", m * 1e-10)

    val mpk = Unido.create("meripeninkulma", m * 1852)
    val pk = Unido.create("peninkulma", m * 10000)
    val kn = Unido.create("solmu", mpk / h)

    val mi = Unido.create("mile", m * 1609.344)
    val yd = Unido.create("yard", m * 0.9144)
    val in = Unido.create("inch", m * 2.54e-2)
    val ly = Unido.create("lightyear", m * 9461e12)
    val at = Unido.create("technical atmosphere", Pa * 0.980665e5)
    val atm = Unido.create("normal atmosphere", kPa * 101.325)

  }



}
