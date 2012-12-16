# Argonaut

[![Build Status](https://travis-ci.org/markhibberd/argonaut.png)](https://travis-ci.org/markhibberd/argonaut)


### What is Argonaut?

Argonaut is a JSON library built using Scala. It provides functionality to specify how to convert to and from JSON for an arbitrary Scala object. It relies on the Scala parser libraries (`util.parsing.combinator`) to read an arbitrary string into a JSON data type, which is known to be slow. Argonaut is licenced under BSD3 (see `etc/LICENCE`).

The JSON data type also implements a large combinator library in an effort to make it easy to use, however, much of this library would be better implemented by exploiting partial lenses and a zipper.

Argonaut is intended for simple applications that use specification-compliant JSON. Argonaut is released under a [BSD3 open-source licence](http://www.opensource.org/licenses/BSD-3-Clause).

### Features

* Lenses and partial lenses for product and sum types, in particular for viewing and updating values of the `Json` sum type.
* A zipper (`com.ephox.argonaut.Cursor`) for traversing and updating a `Json` data structure.
* A traversal data structure (`com.ephox.argonaut.Shift`) that tracks its history of visited positions with an underlying cursor.
* A pretty-printer for outputting JSON values.
* A codec for taking values of arbitrary type to and from JSON values.

### SBT Settings

To use argonaut, add the following repositories to your build:

    override lazy val settings =
      super.settings ++
        Seq(resolvers := Seq(
          "mth.io snapshots"  at "http://repo.mth.io/snapshots"
        , "mth.io releases"  at "http://repo.mth.io/releases"
        , "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
        , "releases"  at "http://oss.sonatype.org/content/repositories/releases"
        ))

Note that we are currently using a head version of scalaz-seven until it gets released. But
it should be fine to use scalaz-seven M2+ if required.

Then add argonaut as a dependency:

    "com.ephox" %% "argonaut" % "5.0-SNAPSHOT" withSources


### Provenance

Argonaut was initially developed to support products at [Ephox](http://ephox.com).

The library was open-sourced under a [BSD License](https://github.com/markhibberd/argonaut/blob/master/LICENSE), drawing users, support and improvements from a number of contributors.

The initial developers have since left the employment of Ephox and now maintain this fork `markhibberd/argonaut`.

It is expected that major releases will now come from this repository.
