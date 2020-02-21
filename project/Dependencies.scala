import sbt._

object Dependencies {
  object Plugins {
    val `macro-paradise` = "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
  }

  lazy val scalaReflect = Def.setting{ "org.scala-lang" % "scala-reflect" % Keys.scalaVersion.value }

  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
}
