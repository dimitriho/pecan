name := "Pecan"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("fr.idho.pecan.Analyzer")

scalacOptions ++= Seq("-unchecked", "-deprecation")
