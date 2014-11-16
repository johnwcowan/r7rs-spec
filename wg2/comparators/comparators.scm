(require-library srfi-4)
(require-library numbers)
(require-library srfi-13)
(module comparators ()
  (import scheme chicken)
  (import srfi-4)
  (import numbers)
  (import srfi-13)
  (export comparator? comparator-comparison-procedure? 
          comparator-hash-function?)
  (export boolean-comparator char-comparator char-ci-comparator 
          string-comparator string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator)
  (export default-comparator)
  (export make-comparator make-inexact-real-comparator make-vector-comparator 
          make-bytevector-comparator make-list-comparator
          make-vectorwise-comparator make-listwise-comparator
          make-car-comparator make-cdr-comparator make-pair-comparator
          make-improper-list-comparator make-selecting-comparator
          make-refining-comparator make-reverse-comparator
          make-debug-comparator)
  (export eq-comparator eqv-comparator equal-comparator)
  (export comparator-type-test-procedure comparator-equality-predicate 
          comparator-comparison-procedure comparator-hash-function)
  (export comparator-test-type comparator-check-type comparator-equal? 
          comparator-compare comparator-hash)
  (export make-comparison< make-comparison> make-comparison<=
          make-comparison>= make-comparison=/< make-comparison=/>)
  (export if3 if=? if<? if>? if<=? if>=? if-not=?)
  (export =? <? >? <=? >=?)
  (export make= make<  make> make<= make>=)
  (export in-open-interval? in-closed-interval? in-open-closed-interval? 
          in-closed-open-interval?)
  (export comparator-min comparator-max)
  (export comparator-register-default!)
  (include "r7rs-shim.scm")
  (include "basics.scm")
  (include "default.scm")
  (include "constructors.scm")
  (include "advanced.scm"))
