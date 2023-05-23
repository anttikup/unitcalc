import org.scalajs.linker.interface.ModuleSplitStyle

lazy val unitcalc = project.in(file("."))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .settings(
    scalaVersion := "3.2.2",

    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,

    /* Configure Scala.js to emit modules in the optimal way to
     * connect to Vite's incremental reload.
     * - emit ECMAScript modules
     * - emit as many small modules as possible for classes in the "unitcalc" package
     * - emit as few (large) modules as possible for all other classes
     *   (in particular, for the standard library)
     */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("unitcalc")))
    },

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",

    // Depend on Laminar
    libraryDependencies += "com.raquo" %%% "laminar" % "15.0.1",

    // Testing framework
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,

    // Tell ScalablyTyped that we manage `npm install` ourselves
    externalNpm := baseDirectory.value,

    Compile / sourceGenerators += Def.task {
      val inputFile = (Compile / resourceDirectory).value / "inc" / "help.html"
      val html = IO.read(inputFile).trim()
      val outputFile = (Compile / sourceManaged).value / "demo" / "Help.scala"
      IO.write(outputFile, s"""package unitcalc
      object Help { val text = \"\"\"$html\"\"\" }""")
      Seq(outputFile)
    }.taskValue
  )
