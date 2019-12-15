val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pfds-dotty",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
    )
  )
