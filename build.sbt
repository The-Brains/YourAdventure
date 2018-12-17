val v = IO.readLines(new File("VERSION")).head
version := v

val projectName = IO.readLines(new File("PROJECT_NAME")).head
name := projectName

scalaVersion := "2.12.8"

scalacOptions ++= List(
  "-Yrangepos",
  "-feature",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-deprecation",
  "-encoding",
  "utf8"
)

// Code Style
lazy val scalafmtSettings = Seq(
  scalafmtOnCompile := true
)
libraryDependencies += compilerPlugin(scalafixSemanticdb)
scalastyleFailOnError   := true
scalastyleFailOnWarning := true

addCommandAlias("fix", "; compile:scalafix; test:scalafix")
addCommandAlias("fixCheck", "; compile:scalafix --check; test:scalafix --check")

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

addCommandAlias("styleCheck", "; compile:scalastyle; test:scalastyle")

addCommandAlias("checkAll", "; fixCheck; fmtCheck; styleCheck")
//////

// SCALAZ
val scalaZVersion = "7.2.27"
libraryDependencies += "org.scalaz" %% "scalaz-core"   % scalaZVersion
libraryDependencies += "org.scalaz" %% "scalaz-effect" % scalaZVersion
//////////////

// JLINE3
// https://github.com/jline/jline3
libraryDependencies += "org.jline" % "jline" % "3.9.0"
/////////////////////////////////

// ////////////scallop/////////////////
libraryDependencies ++= Seq(
  // https://github.com/scallop/scallop
  "org.rogach" %% "scallop" % "3.1.2"
)
/////////////////////////////////////

// ////////////CIRCE/////////////////
val circeVersion = "0.10.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion
)
/////////////////////////////////////

// ////////////Test/////////////////
// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)
parallelExecution in Test := false
/////////////////////////////////////

///// DOCKER BUILD /////
// https://github.com/Demandbase/sbt_safety_plugin#for-fat-jar-assembly-build
test in assembly := {}

// Docker settings - http://www.scala-sbt.org/sbt-native-packager/formats/docker.html
enablePlugins(DockerPlugin)

imageNames in docker := Seq(
  // Sets the latest tag
  ImageName(s"${organization.value}/${name.value}:latest"),
  // Sets a name with a tag that contains the project version
  ImageName(
    namespace = Some(organization.value),
    repository = name.value,
    tag = Some(version.value)
  )
)
dockerfile in docker := {
  // The assembly task generates a fat JAR file
  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  new Dockerfile {
    from("demandbase/ivy2cache:latest")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", artifactTargetPath)
  }
}

version in docker      := version.value
buildOptions in docker := BuildOptions(cache = false)
/////////////////////////////////////
