name := "telegaBot"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "com.bot4s" %% "telegram-core" % "5.0.3",
  "org.typelevel" %% "cats-core" % "2.3.0",
  "org.typelevel" %% "cats-effect" % "2.5.3",
  "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats" % "3.2.3",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
  "com.typesafe" % "config" % "1.4.1"
)

enablePlugins(JavaAppPackaging)