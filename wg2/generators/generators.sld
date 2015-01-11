(define-library (generators)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme read))
  (import (comparators))
  (export make-generator make-circular-generator make-iota-generator
          make-range-generator make-tabulation-generator make-coroutine-generator
          make-list-generator make-vector-generator make-reverse-vector-generator
          make-string-generator make-bytevector-generator make-bits-generator
          make-port-generator make-port-sexp-generator make-port-line-generator
          make-port-char-generator make-port-byte-generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gconcatenate gmerge gunion gintersection gmap gfold
          gfilter gremove gfilter-map gstate-filter gbuffer-filter
          gtake gdrop gtake-while gdrop-while gpairs gtuple glists gvectors gstrings
          gdelete gdelete-neighbor-dups)
  (export generator->list generator->reverse-list generator-fold generator-for-each
          generator-collect generator-last generator-find generator-length
          generator-count generator-any generator-every generator=? generator-unfold)
  (include "generators-impl.scm")
)
