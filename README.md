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

Just add argonaut as a dependency (6.0-M1 is the current version available on oss.sonatype.org at the moment).

Stable:

    "io.argonaut" %% "argonaut" % "6.0-M1"

Latest:

    "io.argonaut" %% "argonaut" % "6.0-SNAPSHOT" changing()


Note that argonaut depends on scalaz 7.0-M8.


### Release

Add to `~/.sbt/0.12.1/sonatype.sbt`


    credentials += Credentials("Sonatype Nexus Repository Manager",
                               "oss.sonatype.org",
                               "<username>",
                               "<password>")


Run:
    ./sbt "+publish"

### Provenance

Argonaut was initially developed to support products at [Ephox](http://ephox.com).

The library was open-sourced under a [BSD License](https://github.com/markhibberd/argonaut/blob/master/LICENSE), drawing users, support and improvements from a number of contributors.

The initial developers have since left the employment of Ephox and now maintain this fork `markhibberd/argonaut`.

It is expected that major releases will now come from this repository.
