(use test)
(use genequal)
(include "srfi-4-shim.scm")

(test-group "genequal"
  (test-group "genequal/eqv"
    (define (neqv-comparator x y c) (not (eqv? x y)))
    (test-assert (generalized-equal? #t #t))
    (test-assert (not (generalized-equal? #f #t)))
    ;; consistent with eqv?
    (test-assert (generalized-equal? #t #t neqv-comparator))
    (test-assert (generalized-equal? #t #f neqv-comparator))
  ) ; end genequal/eqv

  (test-group "genequal/atomic"
    (test-assert (numeric-comparator 2 2 '()))
    (test-assert (numeric-comparator 2 2.0 '()))
    (test-assert (not (numeric-comparator 2.0 3.0 '())))
    (test 'pass (numeric-comparator 0 #f '()))
    (test 'pass (numeric-comparator #t 0 '()))
    (test-assert (char-ci-comparator #\f #\f '()))
    (test-assert (char-ci-comparator #\f #\F '()))
    (test-assert (not (char-ci-comparator #\F #\G '())))
    (test 'pass (char-ci-comparator #\F 0 '()))
    (test 'pass (char-ci-comparator 0 #\F '()))
    (test-assert (string-ci-comparator "foo" "foo" '()))
    (test-assert (string-ci-comparator "foo" "Foo" '()))
    (test-assert (not (string-ci-comparator "Foo" "Bar" '())))
    (test 'pass (string-ci-comparator "Foo" 32 '()))
    (test 'pass (string-ci-comparator 47 "Bar" '()))
  ) ; end genequal/atomic

  (test-group "genequal/list"
    (define pair '(a . b))
    (define abc '(a b c))
    (define abc2 '(a b c))
    (define def '(d e f))
    (define deg '(d e g))
    (define defg '(d e f g))
    (test-assert (list-comparator '() '() '()))
    (test-assert (not (list-comparator '() 23 '())))
    (test-assert (not (list-comparator 47 '() '())))
    (test 'pass (list-comparator 32 pair '()))
    (test 'pass (list-comparator pair 47 '()))
    (test-assert (list-comparator abc abc2 '()))
    (test-assert (not (list-comparator abc def '())))
    (test-assert (not (list-comparator def defg '())))
    (test-assert (not (list-comparator def deg '())))
  ) ; end genequal/list

  (test-group "genequal/vector"
    (define abc '#(a b c))
    (define abc2 '#(a b c))
    (define def '#(d e f))
    (define defg '#(d e f g))
    (define deg '#(d e g))
    (test-assert (vector-comparator abc abc2 '()))
    (test-assert (not (vector-comparator abc def '())))
    (test-assert (not (vector-comparator abc def '())))
    (test-assert (not (vector-comparator def defg '())))
    (test-assert (not (vector-comparator def deg '())))
  ) ; end genequal/vector

  (test-group "genequal/bytevector"
    (define abc (bytevector 1 2 3))
    (define abc2 (bytevector 1 2 3))
    (define def (bytevector 4 5 6))
    (define defg (bytevector 4 5 6 7))
    (define deg (bytevector 4 5 7))
    (test-assert (bytevector-comparator abc abc2 '()))
    (test-assert (not (bytevector-comparator abc def '())))
    (test-assert (not (bytevector-comparator abc def '())))
    (test-assert (not (bytevector-comparator def defg '())))
    (test-assert (not (bytevector-comparator def deg '())))
  ) ; end genequal/bytevector

  (test-group "genequal/equalp"
    (define equal?? (make-specific-equality
       numeric-comparator char-ci-comparator string-ci-comparator))
    (test-assert (equal?? "Foo" "foo"))
    (test-assert (equal?? '("Foo" "BAR" "baZ") '("foo" "bar" "baz")))
    (test-assert (equal?? '#(2 3) '#(2.0 3.0)))
    (test-assert (not (equal?? "Foo" "Bar")))
    (test-assert (not (equal?? '("Foo" "Bar") '("foo" "baz"))))
    (test-assert (not (equal?? '#(2 3) '#(3.0 2.0))))
  ) ; end genequal/equalp

) ; end genequal

(test-exit)
