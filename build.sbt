name := "gbb"

version := "0.3"

scalaVersion := "2.10.2"

//autoCompilerPlugins := true

fork in run := true


//addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")


libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies ++= {
  val liftVersion = "2.5.1"
  val liftVersionForFobo = "2.5"
  Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile",
    "org.eclipse.jetty" % "jetty-webapp" % "8.1.7.v20120910"  % "container,test",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,compile" artifacts Artifact("javax.servlet", "jar", "jar"),
    "net.liftmodules" %% ("fobo_"+liftVersionForFobo) % "1.0"
  )
}

//libraryDependencies += "cc.co.scala-reactive" %% "reactive-web" % "0.3.2.1"

libraryDependencies += "com.typesafe" %% "scalalogging-slf4j" % "1.0.1"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"

libraryDependencies += "org.slf4j" % "log4j-over-slf4j" % "1.6.1"

libraryDependencies += "ch.qos.logback" % "logback-core" % "0.9.24" % "compile"

libraryDependencies +=  "ch.qos.logback" % "logback-classic" % "0.9.24" % "compile" 

libraryDependencies +=  "org.apache.tika" % "tika-core" % "1.4"

libraryDependencies +=  "org.apache.tika" % "tika-parsers" % "1.4"

libraryDependencies +=  "org.imgscalr" % "imgscalr-lib" % "4.2"

libraryDependencies += "net.java.xadisk" % "xadisk" % "1.2.1"

libraryDependencies += "javax.resource" % "connector-api" % "1.5" // for xadisk

//libraryDependencies += "javaee" % "javaee-api" % "5" //for xadisk

libraryDependencies += "org.codehaus.btm" % "btm" % "2.1.3"

libraryDependencies += "org.bouncycastle" % "bcprov-jdk16" % "1.46"

libraryDependencies += "org.bouncycastle" % "bcmail-jdk16" % "1.46"

//libraryDependencies += "com.twitter" %% "util-collection" % "6.5.0"

libraryDependencies ++= {
  val version = "2.4"
   Seq(
  "commons-io" % "commons-io" % version
  )
}

libraryDependencies ++= {
    val akkaVersion = "2.2.0"
    val h2Version = "1.3.171"
    val slickVersion = "1.0.1"
    Seq(
        //"com.typesafe.akka" %% "akka-actor" % akkaVersion,
        "com.h2database" % "h2" % h2Version,
        "com.typesafe.slick" %% "slick" % slickVersion
        //"org.slf4j" % "slf4j-nop" % "1.6.4"
    )
}


libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.3"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.4.2"

seq(webSettings :_*)

port in container.Configuration := 8888
