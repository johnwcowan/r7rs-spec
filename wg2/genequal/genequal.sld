(define-library (genequal)
  (import (scheme base))
  (export generalized-equal? make-atomic-comparator make-specific-equality)
  (export numeric-comparator char-ci-comparator
          list-comparator string-comparator string-ci-comparator
          vector-comparator bytevector-comparator)
  (include "genequal.scm")
)
