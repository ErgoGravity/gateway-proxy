
name := "gatewayproxy"

version := "1.0"

lazy val `gatewayproxy` = (project in file(".")).enablePlugins(PlayScala)

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "SonaType" at "https://oss.sonatype.org/content/groups/public",
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

scalaVersion := "2.12.10"

libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice )
libraryDependencies ++= Seq(
  "org.ergoplatform" %% "ergo-appkit" % "develop-dd40e4e5-SNAPSHOT",
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
