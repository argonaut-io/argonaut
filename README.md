# Argonaut

[![argonaut Scala version support](https://index.scala-lang.org/argonaut-io/argonaut/argonaut/latest-by-scala-version.svg)](https://index.scala-lang.org/argonaut-io/argonaut/argonaut)


### What is Argonaut?

Argonaut is a JSON library for Scala, providing a rich library for parsing, printing and manipulation as well as convenient codecs for translation to and from scala data types.

Argonaut is licenced under BSD3 (see `LICENCE`).


### Documentation

* [Scala Docs](https://javadoc.io/doc/io.github.argonaut-io/argonaut_2.13/latest/index.html)
* [Examples](https://github.com/argonaut-io/argonaut/tree/master/argonaut/shared/src/test/scala/argonaut/example)


### SBT Settings

Just add argonaut as a dependency.

Stable:

```scala
libraryDependencies += "io.github.argonaut-io" %% "argonaut" % "6.3.11"
```

Note that the 6.2.x releases supports scala `2.11.*`, `2.12.*` and `2.13.*` with scalaz `7.2.*`.

Note that the 6.3.x releases supports scala `2.12.*`, `2.13.*` and `3.x` with scalaz `7.3.*`.


### Release

For a snapshot build run:
    ./sbt +publish

For a release build run:

    ./sbt "release cross"

Note for a release build you will want to enter the details for the
release build number and then the subsequent build number. At this
step it is fine to enter the original build number as the next number
(for example when doing Milestone or RC builds). As an example:

    Release version [6.0] : 6.0-M3
    Next version [6.1-SNAPSHOT] : 6.0-SNAPSHOT


### Provenance

Argonaut was initially developed to support products at [Ephox](http://ephox.com), who have now kindly relinquished control to the community.

The library was open-sourced under a [BSD License](https://github.com/argonaut-io/argonaut/blob/master/LICENSE), drawing users, support and improvements from a number of contributors.
