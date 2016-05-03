import java.io.File

import sbt._
import Keys._
import org.scalatra.sbt._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import com.earldouglas.xwp._
import sbt.Keys._
import sbt._


object Lucie extends Build  {

  lazy val monocleVersion = "1.2.0"

  lazy val commonSettings = Seq(
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.8",
    organization := "fr.geocites.lucie",
    resolvers ++=
      Seq(
        Resolver.sonatypeRepo("snapshots"),
        "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
      ),
    libraryDependencies ++= Seq (
      "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
      "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
      "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

  lazy val model = Project("model", file("model")) settings(commonSettings: _*) settings (
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6",
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.15.0",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"
  ) dependsOn(data)

  lazy val data = Project("data", file("data")) settings(commonSettings: _*) enablePlugins (ScalaJSPlugin)

  val scalatraVersion = "2.4.0"
  val jettyVersion = "9.3.7.v20160115"
  val json4sVersion = "3.3.0"
  val scalatagsVersion = "0.5.4"

  lazy val website = taskKey[Unit]("website")

  lazy val shared = Project("gui-shared", file("gui/shared")) settings(commonSettings: _*) dependsOn(data) enablePlugins (ScalaJSPlugin)

  lazy val client = Project(
    "gui-client",
    file("gui/client")) settings(commonSettings: _*) settings(
      skip in packageJSDependencies := false,
      jsDependencies += "org.webjars" % "d3js" % "3.5.12" / "d3.min.js",
      jsDependencies += "com.github.yoeluk" %%% "paper-scala-js" % "0.5-SNAPSHOT" / "paper-full.min.js",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "autowire" % "0.2.5",
        "com.lihaoyi" %%% "upickle" % "0.3.8",
        "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
        "com.lihaoyi" %%% "scalarx" % "0.2.9",
        "fr.iscpif" %%% "scaladget" % "0.8.0-SNAPSHOT",
        "org.scala-js" %%% "scalajs-dom" % "0.8.2",
        "org.json4s" %% "json4s-jackson" % json4sVersion
      )
    ) dependsOn(shared) enablePlugins (ScalaJSPlugin)

  lazy val server = Project(
    "gui-server",
    file("gui/server")) settings(commonSettings: _*) settings(ScalatraPlugin.scalatraWithJRebel: _*) settings (
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "autowire" % "0.2.5",
        "com.lihaoyi" %% "upickle" % "0.3.8",
        "com.lihaoyi" %% "scalatags" % scalatagsVersion,
        "org.scalatra" %% "scalatra" % scalatraVersion,
        "ch.qos.logback" % "logback-classic" % "1.1.3" % "runtime",
        "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
        "org.eclipse.jetty" % "jetty-webapp" % jettyVersion % "container"
      )
    ) dependsOn(shared, model) enablePlugins (JettyPlugin)


  lazy val bootstrap = Project(
    "gui-bootstrap",
    file("gui/target/bootstrap")) settings(commonSettings: _*) settings(
      (website <<= (fullOptJS in client in Compile, resourceDirectory in client in Compile, target in server in Compile) map { (ct, r, st) =>
        copy(ct, r, new File(st, "webapp"))})) dependsOn(client, server)


  private def copy(clientTarget: Attributed[File], resources: File, webappServerTarget: File) = {
    clientTarget.map { ct =>
      val depName = ct.getName.replace("opt.js", "jsdeps.min.js")
      recursiveCopy(new File(resources, "webapp"), webappServerTarget)
      recursiveCopy(ct, new File(webappServerTarget, "js/" + ct.getName))
      recursiveCopy(new File(ct.getParent, depName), new File(webappServerTarget, "js/" + depName))
    }
  }

  private def recursiveCopy(from: File, to: File): Unit = {
    if (from.isDirectory) {
      to.mkdirs()
      for {
        f ← from.listFiles()
      } recursiveCopy(f, new File(to, f.getName))
    }
    else if (!to.exists() || from.lastModified() > to.lastModified) {
      from.getParentFile.mkdirs
      IO.copyFile(from, to, preserveLastModified = true)
    }
  }

}
