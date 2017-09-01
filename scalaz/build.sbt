name := "BeginningScalaz"

organization := "com.bhta"

version := "0.0.1"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.6.0", // command line parsing
  "com.lihaoyi" % "ammonite" % "1.0.2" cross CrossVersion.full,
  "org.scalaz" %% "scalaz-core" % "7.2.15",
  "org.scalaz" %% "scalaz-effect" % "7.2.15",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.15",
  "ch.qos.logback" % "logback-classic" % "1.2.3", // logging
  "org.typelevel" %% "squants" % "1.3.0", // Handling Quantities
  "com.typesafe" % "config" % "1.3.1",    // Handle Configuration
  "org.specs2" %% "specs2-core" % "3.9.5" % "test", // Specs Testing
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test" // Property Based Testing
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq(
  "-Yrangepos",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-Ywarn-dead-code",
  "-Xlint"
    // , "-Xfatal-warnings"
)

scalacOptions in Test ++= Seq(
  "-Yrangepos",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-Ywarn-dead-code",
  "-Xlint"
    //, "-Xfatal-warnings"
)

// Read here for optional dependencies:
// https://etorreborre.github.io/specs2/guide/SPECS2-3.8.3/org.specs2.guide.Installation.html#other-dependencies

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

initialCommands := "import com.bhta._"
