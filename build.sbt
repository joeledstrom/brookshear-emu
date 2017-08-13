// Turn this project into a Scala.js project by importing these settings
enablePlugins(ScalaJSPlugin)

name := "brookshear-emu"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

scalaJSUseMainModuleInitializer := true

// Specify additional .js file to be passed to package-js and optimize-js
//unmanagedSources in (Compile, ScalaJSKeys.packageJS) +=
//    baseDirectory.value / "js" / "startup.js"
