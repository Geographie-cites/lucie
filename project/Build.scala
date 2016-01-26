import sbt.Keys._
import sbt._

object Lucie extends Build {

  lazy val monocleVersion = "1.2.0"

  lazy val root = Project("lucie", file(".")) settings (
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.7",
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.5.0",
    libraryDependencies += "com.codecommit" %% "emm-scalaz" % "0.1-7dd2fd8",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6",
    libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"
  )

}
