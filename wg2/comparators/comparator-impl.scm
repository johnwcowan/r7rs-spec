;;;; Comparators

;;; Record-type definition

;; A comparator is a record of four procedures: a type-tester, an
;; equality procedure, a compare procedure (returns -1, 0, or 1), and
;; a hash function.  It's up to the programmer to make these consistent,
;; although you can pass a comparator to debug-comparator to get one
;; that does dynamic self-testing.

(define-record-type &comparator
  (raw-make-comparator tester equality compare hash cache)
  comparator?
  (tester tester)
  (equality equality)
  (compare compare)
  (hash hash)
  (cache cache set-cache!))

;;; Syntax (must be defined before use, by R7RS rules)

;; Arithmetic IF
(define-syntax if3
  (syntax-rules ()
    ((if3 expr lt eq gt)
     (case expr
       ((1) gt)
       ((0) eq)
       ((-1) lt)
       (else (error "if3: invalid compare value"))))))

;; Internal: macro to return an undefined value (avoids clutter)
(define-syntax undef
  (syntax-rules ()
    ((undef) (if #f #f))))

;; Comparator IFs
(define-syntax if=?
  (syntax-rules ()
    ((if=? expr then)
     (if3 expr undef then undef))
    ((if=? expr then else)
     (if3 expr else then else))))

(define-syntax if<?
  (syntax-rules ()
    ((if<? expr then)
     (if3 expr then undef undef))
    ((if<? expr then else)
     (if3 expr then else else))))

(define-syntax if>?
  (syntax-rules ()
    ((if>? expr then)
     (if3 expr undef undef then))
    ((if>? expr then else)
     (if3 expr else else then))))

(define-syntax if<=?
  (syntax-rules ()
    ((if<=? expr then)
     (if3 expr then then undef))
    ((if<=? expr then else)
     (if3 expr then then else))))

(define-syntax if>=?
  (syntax-rules ()
    ((if>=? expr then)
     (if3 expr undef then then))
    ((if>=? expr then else)
     (if3 expr else then then))))

(define-syntax if-not=?
  (syntax-rules ()
    ((if-not=? expr then)
     (if3 expr then undef then))
    ((if-not=? expr then else)
     (if3 expr then else then))))

;;; Primitive constructor, accessors, and applicators

;; Internal: unique value of cache when empty
(define empty (copy-string "empty"))

;; Safe version of make-comparator that handles #f arguments
(define (make-comparator tester equality compare hash)
  (raw-make-comparator
    (if tester tester (lambda (x) #t))
    (if equality equality (lambda (x y) (= 0 (compare x y))))
    (if compare compare (lambda (x y) (error "incomparable" x y)))
    (if hash hash (lambda (x) (error "unhashable" x)))
    empty))

;; Return the four procedures of a comparator
(define (comparator-type-test-procedure c)
  (type-test c))

(define (comparator-equality-predicate c)
  (equality c))

(define (comparator-compare-procedure c)
  (compare c))

(define (comparator-hash-function c)
  (hash c))

;; Invoke the four procedures of a comparator
(define (comparator-test-type c x)
  ((type-test c) x))

(define (comparator-check-type c x)
  (if ((type-test c) x)
    #t
    (error "comparator type check failed" c x)))

(define (comparator-equal? c x y)
  ((equality c) x y))

(define (comparator-compare c x y)
  ((compare c) x y))

(define (comparator-hash c x)
  ((comparator c) x))

;;; Compare procedure constructors

(define (make-compare< <)
  (lambda (x y)
    (cond
      ((< x y) -1)
      ((< y x) 1)
      (else 0))))

(define (make-compare> >)
  (lambda (x y)
    (cond
      ((> x y) 1)
      ((> y x) -1)
      (else 0))))

(define (make-compare<= <=)
  (lambda (x y)
    (let ((x<=y (<= x y))
          (y<=x (<= y x)))
      (cond
        ((and x<=y y<=x) 0)
        (x<=y -1)
        (else 1)))))

(define (make-compare>= >=)
  (lambda (x y)
    (let ((x>=y (>= x y))
          (y>=x (>= y x)))
      (cond
        ((and x>=y y>=x) 0)
        (x>=y 1)
        (else -1)))))

(define (make-compare=/< = <)
  (lambda (x y)
    (cond
      ((= x y) 0)
      ((< x y) -1)
      (else 1))))

(define (make-compare=/> = >)
  (lambda (x y)
    (cond
      ((= x y) 0)
      ((> x y) 1)
      (else -1))))

;;; Standard atomic comparators

;; Internal: boolean compare procedure
(define (boolean-compare x y)
  (cond
    ((boolean=? x y) 0)
    (x 1)
    (else -1)))

;; Internal: boolean hash function
(define (boolean-hash x) (if x 1 0))

;; Internal: complex compare function (lexicographic)
(define (complex-compare x y)
  (if (and (real? x) (real? y))
    (real-compare x y)
    (let ((r (real-compare (real-part x) (real-part y))))
      (if (= r 0)
        (real-compare (imag-part x) (imag-part y))
        r))))

;; Internal: real-only compare function
(define real-compare (make-compare=/< = <))

;; Internal: numeric hash function
(define (numeric-hash x)
  (if (real? x)
    (exact x)
    (hash-merge (exact (real-part x)) (exact (imag-part x)))))

;; Internal: string hash function
(define (string-hash x)
  (do ((i 0 (+ i 1))
       (r 0 (hash-merge r (string-ref x i))))
      ((= i (string-length x)) r)))

;; Boolean comparator
(define boolean-comparator
  (make-comparator boolean? boolean=? boolean-compare boolean-hash))

;; Character comparator
(define char-comparator
  (make-comparator char? char=?
    (make-compare=/< char=? char<?)
    char->integer))

(define char-ci-comparator
  (make-comparator character? char-ci=?
    (make-compare=/< char-ci=? char-ci<?)
    char->integer))

(define string-comparator
  (make-comparator string? string=?
    (make-compare=/< string=? string<?)
    string-hash))

(define string-ci-comparator
  (make-comparator string? string-ci=?
    (make-compare=/< string-ci=? string-ci<?)
    (lambda (x) (string-hash (string-foldcase x)))

(define symbol-comparator
  (make-comparator symbol? symbol=?
    (make-compare=/<
      symbol=?
      (lambda (x y) (string<? (symbol->string x) (symbol->string y))))
    (lambda (x) (string-hash (symbol->string x)))))

;; Numerical comparators

(define number-comparator
  (make-comparator number? = complex-compare numeric-hash))

(define complex-comparator
  (make-comparator complex? = complex-compare numeric-hash))

(define real-comparator
  (make-comparator real? = real-compare numeric-hash))

(define rational-comparator
  (make-comparator rational? = real-compare numeric-hash))

(define integer-comparator
  (make-comparator integer? = real-compare numeric-hash))

(define exact-integer-comparator
  (make-comparator exact-integer? = real-compare numeric-hash))

;;; Comparison procedures

(define =?
  (case-lambda
    ((x y) (=? default-comparator x y))
    ((c x y) ((comparator-equal? c) x y))
    ((c) (lambda (x y) (=? c x y)))
    (() (lambda (x y) (=? default-comparator x y)))))

(define not=?
  (case-lambda
    ((x y) (not=? default-comparator x y))
    ((c x y) (not ((comparator-equal? c) x y)))
    ((c) (lambda (x y) (not=? c x y)))
    (() (lambda (x y) (not=? default-comparator x y)))))

(define <?
  (case-lambda
    ((x y) (<? default-comparator x y))
    ((c x y) (eqv? -1 ((comparator-compare c) x y)))
    ((c) (lambda (x y) (<? c x y)))
    (() (lambda (x y) (<? default-comparator x y)))))

(define >=?
  (case-lambda
    ((x y) (>=? default-comparator x y))
    ((c x y) (not (eqv? -1 ((comparator-compare c) x y))))
    ((c) (lambda (x y) (>=? c x y)))
    (() (lambda (x y) (>=? default-comparator x y)))))

(define >?
  (case-lambda
    ((x y) (>? default-comparator x y))
    ((c x y) (eqv? 1 ((comparator-compare c) x y)))
    ((c) (lambda (x y) (>? c x y)))
    (() (lambda (x y) (>? default-comparator x y)))))

(define <=?
  (case-lambda
    ((x y) (<=? default-comparator x y))
    ((c x y) (not (eqv? 1 ((comparator-compare c) x y))))
    ((c) (lambda (x y) (<=? c x y)))
    (() (lambda (x y) (<=? default-comparator x y)))))

(define in-open-interval?
  (case-lambda
    ((x y z) (in-open-interval? default-comparator x y z))
    ((c x y) (and (<? c x y) (<? c y z)))))

(define in-open-closed-interval?
  (case-lambda
    ((x y z) (in-open-closed-interval? default-comparator x y z))
    ((c x y) (and (<? c x y) (<=? c y z)))))

(define in-closed-open-interval?
  (case-lambda
    ((x y z) (in-closed-open-interval? default-comparator x y z))
    ((c x y) (and (<=? c x y) (<? c y z)))))

(define in-closed-interval?
  (case-lambda
    ((x y z) (in-closed-interval? default-comparator x y z))
    ((c x y) (and (<=? c x y) (<=? c y z)))))

;;; Convenience comparators

(define list-comparator (make-list-comparator default-comparator))
(define vector-comparator (make-vector-comparator default-comparator))
(define bytevector-comparator (make-bytevector-comparator default-comparator))
(define eq-comparator (make-comparator #f eq? default-compare default-hash))
(define eqv-comparator (make-comparator #f eqv? default-compare default-hash))
(define equal-comparator (make-comparator #f equal? default-compare default-hash))

;;; Selecting and refining comparators

;; Selecting comparator: uses the first comparator that passes the type test
(define (make-selecting-comparator . cs)
  (make-comparator
    (lambda (x) (selecting-tester x cs))
    (lambda (x y) (selecting-equality x y cs))
    (lambda (x y) (selecting-compare x y cs))
    (lambda (x y) (selecting-hash x cs))))

;; Internal: tester for selecting comparator
(define (selecting-tester x cs)
  (let loop ((cs cs))
    (cond
      ((null? cs) #f)
      (((tester (car cs)) x) #t)
      (else (loop (cdr cs))))))

;; Internal: equality for selecting comparator
(define (selecting-equality x y cs)
  (let loop ((cs cs))
    (cond
      ((null? cs) (error "selecting comparator failure" x y))
      ((and ((tester (car cs)) x) ((tester (car cs)) y))
       ((equality (car cs)) x y))
      (else (loop (cdr cs))))))

;; Internal: compare for selecting comparator
(define (selecting-compare x y cs)
  (let loop ((cs cs))
    (cond
      ((null? cs) (error "selecting comparator failure" x y))
      ((and ((tester (car cs)) x) ((tester (car cs)) y))
       ((equality (car cs)) x y))
      (else (loop (cdr cs))))))

;; Internal: hash for selecting comparator
(define (selecting-hash x cs)
  (let loop ((cs cs))
    (cond
      ((null? cs) (error "selecting comparator failure" x))
      (((tester (car cs)) x) ((hash (car cs)) x))
      (else (loop (cdr cs))))))

;; Refining comparator: compare procedure is lexicographic,
;; the hash function merges all relevant hash function results
(define (make-refining-comparator . cs)
  (make-comparator
    (lambda (x) (selecting-tester x cs))
    (lambda (x y) (selecting-equality x y cs))
    (lambda (x y) (refining-compare x y cs))
    (lambda (x y) (refining-hash x cs))))

;; Internal: compare procedure for refining comparator
(define (refining-compare x y cs)
  (if (and (selecting-tester x cs) (selecting-tester y cs))
    #t
    (error "refining comparator failure" x y))
  (let loop ((cs cs))
    (cond
      ((null? cs) 0)
      ((and ((tester (car cs)) x) ((tester (car cs)) y))
       (let ((r ((equality (car cs)) x y)))
         (if (= r 0)
           (refining-compare x y (cdr cs))
           r)))
      (else (loop (cdr cs))))))

;; Internal: hash function for refining comparator
(define (refining-hash x cs)
  (if (selecting-tester x cs)
    #t
    (error "refining comparator failure" x))
  (let loop ((cs cs) (r 0))
    (cond
      ((null? cs) r)
      (((tester (car cs)) x)
       (loop (cdr cs) (hash-merge r ((hash (car cs)) x))))
      (else (loop (cdr cs) r)))))

;;; Comparator constructors for specialized types

(define (make-car-comparator c)
  (make-comparator
    pair?
    (lambda (x y) (comparator-equal? c (car x) (car y)))
    (lambda (x y) (comparator-compare c (car x) (car y)))
    (lambda (x) (comparator-hash c (car x)))))

(define (make-cdr-comparator c)
  (make-comparator
    pair?
    (lambda (x y) (comparator-equal? c (cdr x) (cdr y)))
    (lambda (x y) (comparator-compare c (cdr x) (cdr y)))
    (lambda (x) (comparator-hash c (cdr x)))))

