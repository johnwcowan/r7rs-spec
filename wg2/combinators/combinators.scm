(module combinators ()
  (import scheme)
  (import (only chicken include))
  (export constantly complement compose simple-compose
          conjoin disjoin each flip all-of? any-of?
          map-reduce always never identity)
  (include "combinators-impl.scm")
)
