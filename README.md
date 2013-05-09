# Argonaut

[![Build Status](https://travis-ci.org/markhibberd/argonaut.png)](https://travis-ci.org/markhibberd/argonaut)



### What is Argonaut?

Argonaut is a JSON library built using Scala. It provides functionality to specify how to convert to and from JSON for an arbitrary Scala object. Argonaut is licenced under BSD3 (see `LICENCE`). See more at [http://argonaut.io](http://argonaut.io).


### Features

* Lenses and partial lenses for product and sum types, in particular for viewing and updating values of the `Json` sum type.
* A zipper (`argonaut.Cursor`) for traversing and updating a `Json` data structure.
* A traversal data structure (`argonaut.Shift`) that tracks its history of visited positions with an underlying cursor.
* A pretty-printer for outputting JSON values.
* A codec for taking values of arbitrary type to and from JSON values.


### Documentation

* [User Docs](http://argonaut.io/doc/)
* [Scala Docs](http://argonaut.io/scaladocs/)
* [Examples](https://github.com/markhibberd/argonaut/tree/master/src/test/scala/argonaut/example)


### SBT Settings

Just add argonaut as a dependency (6.0-RC1 is the current version available on oss.sonatype.org at the moment).

Stable:

    "io.argonaut" %% "argonaut" % "6.0-RC1"

Latest:

    "io.argonaut" %% "argonaut" % "6.0-SNAPSHOT" changing()



### Release

Add to `~/.sbt/0.12.3/sonatype.sbt`


    credentials += Credentials("Sonatype Nexus Repository Manager",
                               "oss.sonatype.org",
                               "<username>",
                               "<password>")


For a snapshot build run:
    ./sbt "+publish"

For a release build run:

    ./sbt "release cross"

Note for a release build you will want to enter the details for the
release build number and then the subsequent build number. At this
step it is fine to enter the original build number as the next number
(for example when doing Milestone or RC builds). As an example:

    Release version [6.0] : 6.0-M2
    Next version [6.1-SNAPSHOT] : 6.0-SNAPSHOT


### Provenance

Argonaut was initially developed to support products at [Ephox](http://ephox.com).

The library was open-sourced under a [BSD License](https://github.com/markhibberd/argonaut/blob/master/LICENSE), drawing users, support and improvements from a number of contributors.

The initial developers have since left the employment of Ephox and now maintain this fork `markhibberd/argonaut`.

It is expected that major releases will now come from this repository.
