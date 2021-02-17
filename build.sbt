lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    resolvers ++= Seq(
      Resolver.mavenLocal,
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    ),

    scalaVersion := "3.0.0-M3", // "0.21.0-RC1", //"0.22.0-bin-20200114-193f7de-NIGHTLY", //dottyLatestNightlyBuild.get,

    scalacOptions ++= Seq(
      "-language:implicitConversions"
    ),

    libraryDependencies ++= Seq(
      // .excludeAll(ExclusionRule(organization="com.trueaccord.scalapb")
      //("com.lihaoyi" %% "pprint" % "0.5.6").withDottyCompat(scalaVersion.value),
      //("io.getquill" %% "quill-core-portable" % "minor_quat_fixes_3-SNAPSHOT").withDottyCompat(scalaVersion.value),
      //("io.getquill" %% "quill-sql-portable" % "minor_quat_fixes_3-SNAPSHOT").withDottyCompat(scalaVersion.value),
      ////("org.scalameta" %% "scalafmt-dynamic" % "2.7.4").withDottyCompat(scalaVersion.value),
      ////("org.scalameta" %% "scalafmt-cli" % "2.7.4").withDottyCompat(scalaVersion.value),
      ////"org.scala-lang" % "scala3-library_3.0.0-M3" % (scalaVersion.value),

      //"org.scalatest" % "scalatest_3.0.0-M3" % "3.2.3" % "test",
      //"org.scalatest" % "scalatest-mustmatchers_3.0.0-M3" % "3.2.3" % "test"
    )
  )
