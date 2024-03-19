ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "Calendar project"
  )

libraryDependencies += "org.scalafx" % "scalafx_3" % "19.0.0-R30"
libraryDependencies += "org.jfxtras" % "jfxtras-controls" % "8.0-r6"
val AkkaVersion = "2.8.5"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % AkkaVersion
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime
