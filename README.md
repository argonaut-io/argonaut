# Argonaut

[![Build Status](https://travis-ci.org/argonaut-io/argonaut.png)](https://travis-ci.org/argonaut-io/argonaut)


### What is Argonaut?

Argonaut is a JSON library for Scala, providing a rich library for parsing, printing and manipulation as well as convenient codecs for translation to and from scala data types.

Argonaut is licenced under BSD3 (see `LICENCE`). See more at [http://argonaut.io](http://argonaut.io).


### Documentation

* [User Docs](http://argonaut.io/doc/)
* [Scala Docs](http://argonaut.io/scaladocs/)
* [Examples](https://github.com/argonaut-io/argonaut/tree/master/src/test/scala/argonaut/example)


### SBT Settings

Just add argonaut as a dependency.

Stable:

    "io.argonaut" %% "argonaut" % "6.1"

Latest:

    "io.argonaut" %% "argonaut" % "6.2-SNAPSHOT" changing()
    "io.argonaut" %% "argonaut-scalaz" % "6.2-SNAPSHOT" changing()
    "io.argonaut" %% "argonaut-monocle" % "6.2-SNAPSHOT" changing()


Note that the 6.1.x release supports scala 2.10.* and 2.11.* with scalaz 7.1.*.

Note that the 6.2 development stream supports scala 2.10.* and 2.11.* with scalaz 7.1.*.


### Release

Add to `~/.sbt/0.13/sonatype.sbt`


    credentials += Credentials("Sonatype Nexus Repository Manager",
                               "oss.sonatype.org",
                               "<username>",
                               "<password>")


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

Argonaut was initially developed to support products at [Ephox](http://ephox.com).

The library was open-sourced under a [BSD License](https://github.com/argonaut-io/argonaut/blob/master/LICENSE), drawing users, support and improvements from a number of contributors.

The initial developers have since left the employment of Ephox and now maintain this fork `argonaut-io/argonaut`.

It is expected that major releases will now come from this repository.
