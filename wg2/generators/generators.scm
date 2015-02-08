(module generators ()
  (import scheme)
  ;; Provide necessary R7RS-small identifiers
  (import (only chicken
            use include case-lambda call/cc when let-values 
            open-input-string error))
  (import (only extras read-line read-byte))
  (require-library srfi-4)
  (import (only srfi-4 u8vector-ref u8vector-length))
  (include "r7rs-shim.scm")
  (use comparators)
  (use numbers)
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
          generator-count generator-any generator-every generator=?
          generator-unfold generator-nth)
  (include "generators-impl.scm")
)
