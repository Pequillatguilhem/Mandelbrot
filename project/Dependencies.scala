import sbt.librarymanagement.{DependencyBuilders, LibraryManagementSyntax, ModuleID}

object Dependencies extends DependencyBuilders with LibraryManagementSyntax {


  val pureconfigVersion = "0.13.0"
  val logbackVersion = "1.2.3"
  val fs2Version = "3.2.4"
  val utilities: Seq[ModuleID] = Seq(
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version,
    "ch.qos.logback" % "logback-classic" % logbackVersion,
    "com.github.pureconfig" %% "pureconfig" % pureconfigVersion
  )


  val all: Seq[ModuleID] = utilities
}
