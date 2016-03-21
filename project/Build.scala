import sbt.Keys._
import sbt._

object Lucie extends Build {

  lazy val monocleVersion = "1.2.0"

  lazy val root = Project("lucie", file(".")) settings (
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.8",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6",
    libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.15.0",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"
  )

}
