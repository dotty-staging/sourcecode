import sbtcrossproject.{crossProject, CrossType}
import OsgiKeys._

val scala211 = "2.11.12"
val scala212 = "2.12.8"
val scala213 = "2.13.0"
val dotty    = "0.18.0-bin-SNAPSHOT"

inThisBuild(List(
  organization := "com.lihaoyi",
  name := "sourcecode",
  scalaVersion := scala213,
  crossScalaVersions := Seq(scala211, scala212, scala213),
  homepage := Some(url("https://github.com/lihaoyi/sourcecode")),
  licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.html")),
  developers += Developer(
    email = "haoyi.sg@gmail.com",
    id = "lihaoyi",
    name = "Li Haoyi",
    url = url("https://github.com/lihaoyi")
  )
))

skip in publish := true
crossScalaVersions := List() // required for `++2.12.6 test` to ignore native project

def macroDependencies(version: String) =
  Seq(
    "org.scala-lang" % "scala-reflect" % version % "provided",
    "org.scala-lang" % "scala-compiler" % version % "provided"
  )

lazy val sourcecode = (project in file ("sourcecode"))
  .settings(
    libraryDependencies ++= {
      if (isDotty.value) Nil else macroDependencies(scalaVersion.value)
    },
    test in Test := (run in Test).toTask("").value,
    unmanagedSourceDirectories in Compile ++= {
      Seq(baseDirectory.value / "src") ++ (
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(baseDirectory.value / "src-2.x")
        case Some((0, _)) => Seq(baseDirectory.value / "src-dotty")
        case _            => Nil
      })
    },
    unmanagedSourceDirectories in Test ++= {
      Seq(baseDirectory.value / "test" / "src") ++ (
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(baseDirectory.value / "test" / "src-2.x")
        case Some((0, _)) => Seq(baseDirectory.value / "test" / "src-dotty")
        case _            => Nil
      })
    },
    // Osgi settings
    osgiSettings,
    exportPackage := Seq("sourcecode.*"),
    privatePackage := Seq(),
    dynamicImportPackage := Seq("*")
  )
  .enablePlugins(SbtOsgi)
