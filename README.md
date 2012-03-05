## Argonaut

#### What is Argonaut?

Argonaut is a JSON library built using Scala. It provides functionality to specify how to convert to and from JSON for an arbitrary Scala object. It relies on the Scala parser libraries (`util.parsing.combinator`) to read an arbitrary string into a JSON data type, which is known to be slow.

The JSON data type also implements a large combinator library in an effort to make it easy to use, however, much of this library would be better implemented by exploiting partial lenses and a zipper.

Argonaut is intended for simple applications that use specification-compliant JSON. Argonaut is released under a [BSD3 open-source licence](http://www.opensource.org/licenses/BSD-3-Clause).

#### Future Improvements

* Exploit lenses and partial lenses for a more complete and general combinator library.
* Implement a zipper for traversing the JSON data structure.
* Implement a traversal cursor function that tracks its operations using the Writer monad.
* Use a more appropriate parser library (not Scala parsers) to improve performance.
* Implement automated tests for type-class laws (e.g. `ToJson` is the inverse of `FromJson`).

***

**Copyright 2012 Ephox Pty Ltd**
