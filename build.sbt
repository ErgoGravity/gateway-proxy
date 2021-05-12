
name := "gatewayproxy"

version := "1.0"

lazy val `gatewayproxy` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
resolvers += "Akka Snapshot Repository" at "https://repo.akka.io/snapshots/"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Typesafe maven releases" at "https://dl.bintray.com/typesafe/maven-releases/"
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice )
libraryDependencies ++= Seq(
  "org.ergoplatform" %% "ergo-appkit" % "4.0.3",
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "com.dripower" %% "play-circe" % "2712.0"
)

assemblyJarName in assembly := s"${name.value}-${version.value}.jar"

javaOptions in Universal ++= Seq(
  "-Dpidfile.path=/dev/null"
)


assemblyMergeStrategy in assembly := {
  case PathList("reference.conf") => MergeStrategy.concat
  case manifest if manifest.contains("MANIFEST.MF") => MergeStrategy.discard
  case manifest if manifest.contains("module-info.class") => MergeStrategy.discard
  case referenceOverrides if referenceOverrides.contains("reference-overrides.conf") => MergeStrategy.concat
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}


