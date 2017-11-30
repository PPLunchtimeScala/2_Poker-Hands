
name := "wei"

version := "0.1"

scalaVersion := "2.11.8"

resolvers ++= ppResolvers

lazy val scalazV = "7.2.16"

// https://mvnrepository.com/artifact/org.scalaz/scalaz-core
libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazV

// https://mvnrepository.com/artifact/org.scalaz/scalaz-concurrent
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalazV

// https://mvnrepository.com/artifact/org.scalaz/scalaz-effect
libraryDependencies += "org.scalaz" %% "scalaz-effect" % scalazV

lazy val ppResolvers = Seq(
  Resolver.mavenLocal,
  "release-candidates" at "http://artifactory.cmdb.inhouse.paddypower.com:8081/artifactory/release-candidates",
  "libs-release" at "http://artifactory.cmdb.inhouse.paddypower.com:8081/artifactory/libs-release",
  "remote-repos" at "http://artifactory.cmdb.inhouse.paddypower.com:8081/artifactory/remote-repos",
  "plugins-release" at "http://artifactory.cmdb.inhouse.paddypower.com:8081/artifactory/plugins-release",
  "Artima Maven Repository" at "http://repo.artima.com/releases"
)