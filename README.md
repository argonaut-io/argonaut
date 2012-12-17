# Argonaut

[![Build Status](https://travis-ci.org/markhibberd/argonaut.png)](https://travis-ci.org/markhibberd/argonaut)


### What is Argonaut?

Argonaut is a JSON library built using Scala. It provides functionality to specify how to convert to and from JSON for an arbitrary Scala object. Argonaut is licenced under BSD3 (see `etc/LICENCE`).


### Features

* Lenses and partial lenses for product and sum types, in particular for viewing and updating values of the `Json` sum type.
* A zipper (`argonaut.Cursor`) for traversing and updating a `Json` data structure.
* A traversal data structure (`argonaut.Shift`) that tracks its history of visited positions with an underlying cursor.
* A pretty-printer for outputting JSON values.
* A codec for taking values of arbitrary type to and from JSON values.


### SBT Settings

To use argonaut, add the following repositories to your build:
    Seq(
      "mth.io snapshots"  at "http://repo.mth.io/snapshots",
      "mth.io releases"  at "http://repo.mth.io/releases"
    )

Note that we are currently using a head version of scalaz-seven until it gets released. But
it should be fine to use scalaz-seven M2+ if required.

Then add argonaut as a dependency:

    "io.argonaut" %% "argonaut" % "6.0-SNAPSHOT" withSources


### Provenance

Argonaut was initially developed to support products at [Ephox](http://ephox.com).

The library was open-sourced under a [BSD License](https://github.com/markhibberd/argonaut/blob/master/LICENSE), drawing users, support and improvements from a number of contributors.

The initial developers have since left the employment of Ephox and now maintain this fork `markhibberd/argonaut`.

It is expected that major releases will now come from this repository.
