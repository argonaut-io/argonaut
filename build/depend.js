var cleanDirs = ['lib'];

var licenses = "lib/run/licenses/";

var scalazV = "2012-04-16_18-03-13";

var dependencies = [
    // ==== Ephox - Production ====
    // ==== Third Party - Production - REQUIRES LICENSE ====
    { name : "scala",
        repository : "thirdpartyrepo",
        version : "2.9.2",
        source : "scala-2.9.2.zip",
        targets : [
            { name : "scala-2.9.2/doc/LICENSE", path : licenses + "scala" },
            { name : "scala-2.9.2/lib/scala-library.jar", path : "lib" },
            { name : "scala-2.9.2/src/scala-library-src.jar", path : "lib" },
            { name : "scala-2.9.2/*", path : "lib/tools/scala" }
        ],
        executables : [
            "lib/tools/scala/bin/scala",
            "lib/tools/scala/bin/scalac",
            "lib/tools/scala/bin/fsc",
            "lib/tools/scala/bin/scaladoc",
            "lib/tools/scala/bin/scalap"
        ]
    },
    { name: "scalaz",
        repository: "buildrepo2",
        version : "7/" + scalazV,
        source: "./",
        targets: [
            { name: "*.jar", path: "lib" }
        ]
    },
    { name: "scalaz",
        repository: "buildrepo2",
        version : "7/" + scalazV,
        source: "LICENCE",
        targets: [
            { name: "LICENCE", path: licenses + "scalaz" }
        ]
    },
    { name : "scalacheck",
        repository : "thirdpartyrepo",
        version : "2.9.2-1.9",
        source : "scalacheck_2.9.2-1.9.jar",
        targets : [
            { name : "scalacheck_2.9.2-1.9.jar", path : "lib" }
        ]
    },
    { name : "scalacheck",
        repository : "thirdpartyrepo",
        version : "2.9.2-1.9",
        source : "scalacheck_2.9.2-1.9-sources.jar",
        targets : [
            { name : "scalacheck_2.9.2-1.9-sources.jar", path : "lib" }
        ]
    }
];
