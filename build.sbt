
name := "hilbert-curves"

version := "0.1"

scalaVersion := "2.12.7"

assemblyJarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "_fat-executable.jar"

libraryDependencies += "org.processing" % "core" % "3.3.7"