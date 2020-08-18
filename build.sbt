
lazy val root = (project in file("."))
  .settings(
    organization := "hu.agroferr",
    name := "AgroFerrInvoiceTransformer",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.3",
    libraryDependencies ++= Seq(
   	    "org.scala-lang.modules" %% "scala-xml" % "2.0.0-M1",
        "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings"
)
