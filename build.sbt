ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.1"

lazy val root = (project in file("."))
    .settings(
        name := "genesis-scala",
        idePackagePrefix := Some("net.spartanb312.genesis"),
    )

libraryDependencies ++= Seq(
    "org.ow2.asm" % "asm" % "9.7.1",
    "org.ow2.asm" % "asm-tree" % "9.7.1",
    "org.ow2.asm" % "asm-util" % "9.7.1",
    "org.ow2.asm" % "asm-commons" % "9.7.1",
    "org.ow2.asm" % "asm-analysis" % "9.7.1",
)
