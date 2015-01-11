(module generators ()
  (import scheme)
  ;; Provide necessary R7RS-small identifiers
  (import (only chicken
            use include case-lambda call/cc when let-values open-input-string))
  (import (only extras read-line read-byte))
  (require-library srfi-4)
  (import (only srfi-4 u8vector-ref u8vector-length))
  (begin
    (define *eof-object* (read (open-input-string "")))
    (define (eof-object) *eof-object*)
    (define (read-u8 port) (read-byte port))
    (define (bytevector-u8-ref bv i) (u8vector-ref bv i))
    (define (bytevector-length bv) (u8vector-length bv))
  )
  (use comparators)
  (use numbers)
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
