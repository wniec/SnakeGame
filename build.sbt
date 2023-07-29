ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"
Compile / packageBin / mainClass := Some("Main")
Compile / run / mainClass := Some("Main")
lazy val commonSettings = Seq(name := "Snake")
lazy val root = (project in file(".")).settings(commonSettings: _*)
libraryDependencies += "org.scalafx" %% "scalafx" % "19.0.0-R30"
libraryDependencies += "org.diirt.javafx" % "javafx-all" % "3.1.7"
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
assembly / mainClass := Some("Main")
assembly / assemblyJarName := "snake-1.0.jar"