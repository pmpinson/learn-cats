resolvers += Resolver.url("TritonIvy", url("https://artifactory.tritondigital.com/artifactory/ivy"))(Resolver.ivyStylePatterns)
addSbtPlugin("tritondigital.sbt" % "meas-fat-jar-sbt-plugin" % "2.+")