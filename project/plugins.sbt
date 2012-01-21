addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0-M3")

libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-proguard-plugin" % (v+"-0.1.1"))
