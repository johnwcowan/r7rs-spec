(define-library (generators)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme read))
  (import (comparators))
  (export make-generator make-circular-generator make-iota-generator
          make-range-generator make-tabulation-generator make-coroutine-generator
          list->generator vector->generator reverse-vector->generator
          string->generator bytevector->generator list->circular-generator
          make-bits-generator
          make-port-generator make-port-sexp-generator make-port-line-generator
          make-port-char-generator make-port-byte-generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gconcatenate gmerge gunion gintersection gmap gfold
          gfilter gremove gstate-filter gbuffer-filter
          gtake gdrop gtake-while gdrop-while gtuple glists gvectors gstrings
          gdelete gdelete-neighbor-dups
          ggroup-by gindex ginterleave gnth-value gselect)
  (export generator->list generator->reverse-list generator-fold generator-for-each
          generator-collect generator-last generator-find generator-length
          generator-count generator-any generator-every generator=? generator-unfold)
  (include "generators-impl.scm")
)
