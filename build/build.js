var p = Ent.Project.create("argonaut", "external");
p.setVersion(1, 0, 0);

function getVersionString() {
    var v = p.version;
    return "set project.version " + [v.major, v.minor, v.point, v.buildNumber].join(".");
}

p.setConfig({
    command: ["./sbt", getVersionString, "clean", "update", "compile", "test", "package"],
    dist: "target/scala_2.8.0",
    distInclude: "*.jar"
});
