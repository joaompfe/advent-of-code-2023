ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.3.0"

wartremoverErrors ++= Warts.unsafe

lazy val root = (project in file(".")).settings(
  name := "adventofcode",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
  )
)
