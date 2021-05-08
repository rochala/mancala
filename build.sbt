
scalaVersion := "2.13.3"

name := "hello-world"
organization := "ch.epfl.scala"
version := "1.0"

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "com.typesafe" % "config" % "1.4.1"
    )

