var dependencies = [
    { name : "boost",
         repository : "thirdpartyrepo",
         source : "boost.jar",
         targets : [
             {name : "boost.jar", path : "lib/run"},
             {name : "boost.jar", path : "lib/src"}
         ]
    },
    { name:  "circuit",
         repository : "buildrepo2",
         source  : "circuit.zip",
         targets: [
             { name : "circuit.jar", path: "lib/run" }
         ]
    },
    {
        name : "junit",
        repository : "thirdpartyrepo",
        source : "junit*.zip",
        targets : [
            { name : "junit4.8.2/junit-4.8.2.jar", path : "lib/test"}
        ]
    }
];
