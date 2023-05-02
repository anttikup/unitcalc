package unidos.units

import scala.math.{pow, Pi}


object List {
  println("Loading list")

  // Axes
  val dimensionless = Dims.get("Dimensionless").get
  val length = Dims.get("Length").get
  val time = Dims.get("Time").get
  val mass = Dims.get("Mass").get
  val current = Dims.get("ElectricCurrent").get
  val temp = Dims.get("Temperature").get
  val amount = Dims.get("AmountOfSubstance").get
  val intensity = Dims.get("LuminousIntensity").get

  // Quantities
  val freq = Dims.name(dimensionless/time, "frequency")
  val area = Dims.name(length*length, "area")
  val volume = Dims.name(area*length, "volume")
  val speed = Dims.name(length/time, "speed")
  val force = Dims.name(length*mass/(time*time), "force")
  val pressure = Dims.name(mass/length/time/time, "pressure")
  val acceleration = Dims.name(speed/time, "acceleration")
  val planeAngle = Dims.name(dimensionless, "plane angle")
  val solidAngle = Dims.name(dimensionless, "solid angle")

  val energy = Dims.name(force*length, "energy")
  val power = Dims.name(energy/time, "power")
  val charge = Dims.name(current*time, "electric charge")
  val voltage = Dims.name(power/current, "voltage")
  val capacitance = Dims.name(charge/voltage, "capacitance")
  val resistance = Dims.name(voltage/current, "resistance")
  val conductance = Dims.name(dimensionless/resistance, "conductance")
  val magneticFlux = Dims.name(voltage*time, "magnetic flux")
  val magneticFluxDensity = Dims.name(magneticFlux/length/length, "magnetic flux density")
  val inductance = Dims.name(magneticFlux/current, "inductance")
  val lightTodo = Dims.name(intensity*solidAngle, "light todo")
  val lightTodo2 = Dims.name(lightTodo/length/length, "light todo 2")
  val activity = Dims.name(dimensionless/time, "activity")
  val absorbedTodo = Dims.name(energy/mass, "absorbed todo")
  val equivalentTodo = Dims.name(energy/mass, "equivalent todo")
  val catalyticActivity = Dims.name(amount/time, "catalytic activity")

  println(s"pressure: $pressure")

  // Base units
  val `1` = Unido.get("1").get
  val s = Unido.put("second", 1, time)
  val m = Unido.put("metre", 1, length)
  val kg = Unido.put("kilogram", 1, mass)
  val A = Unido.put("ampere", 1, current)
  val K = Unido.put("kelvin", 1, temp)
  val mol = Unido.put("mole", 1, amount)
  val cd = Unido.put("candela", 1, intensity)

  // Derived units
  val rad = Unido.put("radian", 1, planeAngle)
  val sr = Unido.put("steradian", 1, solidAngle)
  val Hz = Unido.put("hertz", `1`/s)
  val N = Unido.put("newton", 1, force)
  val Pa = Unido.put("pascal", 1, pressure)
  val J = Unido.put("joule", N * m)
  val W = Unido.put("watt", J / s)
  val V = Unido.put("volt", W / A)
  val C = Unido.put("coulomb", A * s)
  val `Ω` = Unido.put("ohm", V / A)
  val S = Unido.put("siemens", `1`/`Ω`)
  val Wb = Unido.put("weber", V*s)
  val T = Unido.put("tesla", Wb/m/m)
  val H = Unido.put("henry", Wb/A)
  val lm = Unido.put("lumen", cd*sr)
  val lx = Unido.put("lux", lm/m/m)
  val Bq = Unido.put("becquerel", `1`/s)
  val Gy = Unido.put("gray", J/kg)
  val Sv = Unido.put("sievert", J/kg)
  val kat = Unido.put("katal", mol/s)

  val `m²` = Unido.put("square metre", m * m)
  val `m³` = Unido.put("cubic metre", m * m * m)

  val `m/s` = Unido.put("meters per second", m / s)


  val dm = Unido.put("desimetre", m / 10)
  val kPa = Unido.put("kilopascal", Pa * 1000)
  // Other units
  val min = Unido.put("minute", s * 60)
  val h = Unido.put("hour", min * 60)
  val `°` = Unido.put("degree", rad * (2*Pi/360))
  val `′` = Unido.put("arcminute", `°` / 60)
  val `″` = Unido.put("arcsecond", `′` / 60)
  val gon = Unido.put("gradian", rad * (2*Pi/400))
  val AU = Unido.put("astronomical unit", m * 149597.9e6)
  val pc = Unido.put("parsek", m * 3.085678e16)
  val l = Unido.put("litre", dm * dm * dm)
  val eV = Unido.put("electron volt", J * 1.6021773e-19)
  val u = Unido.put("atomic mass unit", kg * 1.6605402e-27)
  val bar = Unido.put("bar", Pa * 1e5)
  val `Å` = Unido.put("ångström", m * 1e-10)

  val mpk = Unido.put("meripeninkulma", m * 1852)
  val pk = Unido.put("peninkulma", m * 10000)
  val kn = Unido.put("solmu", mpk / h)

  val mi = Unido.put("mile", m * 1609.344)
  val yd = Unido.put("yard", m * 0.9144)
  val in = Unido.put("inch", m * 2.54e-2)
  val ly = Unido.put("lightyear", m * 9461e12)
  val at = Unido.put("technical atmosphere", Pa * 0.980665e5)
  val atm = Unido.put("normal atmosphere", kPa * 101.325)

}
