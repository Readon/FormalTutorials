ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.16"
ThisBuild / organization := "org.example"

val spinalVersion = "1.8.1"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalTester = "com.github.spinalhdl" %% "spinalhdl-tester" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
val orgScalaTest = "org.scalatest" %% "scalatest" % "3.2.5"

lazy val all = (project in file("."))
  .settings(
    name := "FormalTutorial",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalTester, spinalIdslPlugin, orgScalaTest)
  )

fork := true
