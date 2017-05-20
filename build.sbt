name := "SWExam"

version := "0.0.5"

organization := "com.wong"

scalaVersion := "2.10.4"

resolvers ++= Seq("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "staging" at "https://oss.sonatype.org/content/repositories/staging",
  "releases" at "https://oss.sonatype.org/content/repositories/releases"
)

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("heroku-sbt-plugin-releases",
  url("https://dl.bintray.com/heroku/sbt-plugins/"))(Resolver.ivyStylePatterns)

seq(webSettings: _*)

unmanagedResourceDirectories in Test <+= (baseDirectory) {
  _ / "src/main/webapp"
}

enablePlugins(ScalaJSPlugin)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

git.remoteRepo := "git@github.com:ThoughtworksInc/Binding.scala.git"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= {
  val liftVersion = "2.6.2"
  Seq(
"com.thoughtworks.binding" %%% "dom" % "10.0.1",
"com.thoughtworks.binding" %%% "binding" % "10.0.1",
"com.thoughtworks.binding" %%% "futurebinding" % "10.0.1",
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile" withSources ,
    "net.liftweb" %% "lift-mapper" % liftVersion % "compile" withSources,
    "net.liftmodules" %% "fobo_2.6" % "1.5" % "compile" withSources,
    "net.liftmodules"   % "imaging_2.6_2.10" % "1.3" % "compile",
    "net.liftmodules"   % "machine_2.5_2.10" % "1.2"  % "compile",
    "net.liftmodules"   % "mongoauth_2.6_2.10" % "0.5"  % "compile",
    "net.liftmodules"   % "widgets_2.6_2.10" %"1.3" % "compile",
    "org.eclipse.jetty" % "jetty-webapp" % "8.1.17.v20150415" % "container,test",
    "org.eclipse.jetty" % "jetty-plus" % "8.1.17.v20150415" % "container,test", // For Jetty Config
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.specs2" %% "specs2-core" % "3.6.4" % "test",
    "com.h2database" % "h2" % "1.4.187"
  )
}

scalacOptions in Test ++= Seq("-Yrangepos")
