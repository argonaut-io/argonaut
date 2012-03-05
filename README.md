## Argonaut


Argonaut is a JSON library built in Scala. It provides functionality to specify how to convert to and from JSON for an arbitrary Scala object.

### Improvements

* Exploit lenses and partial lenses for a more complete and general combinator library.
* Implement a zipper for traversing the JSON data structure.
* Implement a traversal cursor function that tracks its operations using the Writer monad.
* Use a more appropriate parser library (not Scala parsers) to improve performance.
* Implement automated tests for type-class laws (e.g. `ToJson` is the inverse of `FromJson`).
