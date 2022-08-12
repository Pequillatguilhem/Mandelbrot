name := "Mandelbrot"

version := "0.1"

scalaVersion := "2.13.8"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.mavenLocal
libraryDependencies ++= Dependencies.all
lazy val mandelbrot = project in file(".")
