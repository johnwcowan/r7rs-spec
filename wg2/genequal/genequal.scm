(use srfi-4)
(module genequal ()
  (import scheme chicken srfi-4)
  (export generalized-equal? make-atomic-comparator make-specific-equality)
  (export numeric-comparator char-ci-comparator
          list-comparator string-comparator string-ci-comparator
          vector-comparator bytevector-comparator)
  (include "count.scm")
  (include "srfi-4-shim.scm")
  (include "genequal-impl.scm")
)
