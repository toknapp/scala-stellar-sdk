import sbt.Keys.organization
import scoverage.ScoverageKeys.coverageMinimum

// format: off
lazy val commonSettings = Seq(
  name := "scala-stellar-sdk",
  organization := "io.github.synesso",
  scalaVersion := "2.12.8",
  homepage := Some(url("https://github.com/synesso/scala-stellar-sdk")),
  startYear := Some(2018),
  description := "Perform Stellar (distributed payments platform) operations from your Scala application. " +
    "Build and submit transactions, query the state of the network and stream updates.",
  developers := List(
    Developer("jem", "Jem Mawson", "jem.mawson@gmail.com", url = url("https://keybase.io/jem"))
  ),
  crossScalaVersions := Seq("2.12.8"),
  scalacOptions ++= Seq(
    "-Yrangepos",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-encoding",
    "UTF-8",
    "-target:jvm-1.8"),
  fork := true,
  coverageMinimum := 99,
  coverageFailOnMinimum := true,
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  bintrayRepository := "mvn",
  bintrayPackageLabels := Seq("scala", "stellar"),
  pgpPublicRing := baseDirectory.value / "project" / ".gnupg" / "pubring.gpg",
  pgpSecretRing := baseDirectory.value / "project" / ".gnupg" / "secring.gpg",
  pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray)
)

lazy val root = (project in file("."))
  .enablePlugins(GitVersioning)
  .enablePlugins(SitePlugin).settings(
    siteSourceDirectory := target.value / "paradox" / "site" / "main"
  )
  .enablePlugins(GhpagesPlugin).settings(
    git.remoteRepo := "git@github.com:synesso/scala-stellar-sdk.git"
  )
  .enablePlugins(BuildInfoPlugin).settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "stellar.sdk"
  )
  .enablePlugins(ParadoxPlugin).settings(
    paradoxProperties ++= Map(
      "name" -> name.value,
      "organization" -> organization.value,
      "version" -> version.value,
      "scalaBinaryVersion" -> scalaBinaryVersion.value,
      "scaladoc.stellar.base_url" -> "https://synesso.github.io/scala-stellar-sdk/api"
    )
  )
  .enablePlugins(ParadoxMaterialThemePlugin).settings(
  paradoxMaterialTheme in Compile ~= {
    _
      .withRepository(url("https://github.com/synesso/scala-stellar-sdk").toURI)
      .withSocial(uri("https://github.com/synesso"), uri("https://keybase.io/jem"))
      .withoutSearch()
  }
).configs(IntegrationTest)
  .settings(
    commonSettings,
    compile := ((compile in Compile) dependsOn (paradox in Compile)).value,
    test := ((test in Test) dependsOn dependencyUpdates).value,
    dependencyUpdatesFailBuild := true,
    Defaults.itSettings,
    target in Compile in doc := target.value / "paradox" / "site" / "main" / "api",
    libraryDependencies ++= List(
      "commons-codec" % "commons-codec" % "1.12",
      "net.i2p.crypto" % "eddsa" % "0.3.0",
      "com.typesafe.akka" %% "akka-http" % "10.1.8",
      "com.typesafe.akka" %% "akka-stream" % "2.5.22",
      "com.lightbend.akka" %% "akka-stream-alpakka-sse" % "1.0.0",
      "de.heikoseeberger" %% "akka-http-json4s" % "1.25.2",
      "org.json4s" %% "json4s-native" % "3.6.5",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.typelevel" %% "cats-core" % "1.6.0",
      "tech.sparse" %%  "toml-scala" % "0.2.0",
      "org.specs2" %% "specs2-core" % "4.5.1" % "test,it",
      "org.specs2" %% "specs2-mock" % "4.5.1" % "test",
      "org.specs2" %% "specs2-scalacheck" % "4.5.1" % "test"
    )
  )

