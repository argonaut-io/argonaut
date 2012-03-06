var p = Ent.Project.create("argonaut", "external");
p.setVersion(3, 0, 0);

function getVersionString() {
    var v = p.version;
    return "set version := \"" + [v.major, v.minor, v.point, v.buildNumber].join(".") + "\"";
}

p.setConfig({
    command: ["./sbt", getVersionString, "clean", "update", "compile", "test", "package"],
    dist: "target/scala-2.9.1",
    distInclude: "*.jar"
});
