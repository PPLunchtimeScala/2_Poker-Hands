name := "Poker Hands"

version := "1.0"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "tpolecat" at "http://dl.bintray.com/tpolecat/maven",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
  "ifxjdbc" at "http://artifactory.cmdb.inhouse.paddypower.com:8081/artifactory/remote-repos",
  Resolver.sonatypeRepo("releases")
)


libraryDependencies += "co.fs2" % "fs2-core_2.11" % "0.9.0-M3"

val scalacheck = Seq(
    "org.scalacheck" %% "scalacheck" % "1.12.2"
)

val scalatest = Seq(
    "org.scalatest" %% "scalatest" % "2.2.4"
)

val scalazV = "7.1.3"

val scalazStreamV = "0.8"

val logging = Seq (
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
)

val scalaz = Seq(
  "org.scalaz" %% "scalaz-core" % scalazV,
  "org.scalaz.stream" %% "scalaz-stream" % scalazStreamV
)

val fs2 = Seq(
  "co.fs2" % "fs2-core_2.11" % "0.9.0-M3",
  "co.fs2" % "fs2-io_2.11" % "0.9.0-M3"
)

libraryDependencies ++= scalacheck ++ scalatest ++ scalaz

initialCommands in console := """
    |import scalaz._
    |import Scalaz._
    |import scalaz.concurrent.Task
    |import scala.concurrent.duration._
    |""".stripMargin
