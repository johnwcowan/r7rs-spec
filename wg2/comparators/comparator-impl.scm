;;;; Comparators

;;; Record-type definition

;; A comparator is a record of four procedures: a type-checker, an
;; equality procedure, a compare procedure (returns -1, 0, or 1), and
;; a hash function.  It's up to the programmer to make these consistent,
;; although you can pass a comparator to debug-comparator to get one
;; that does dynamic self-checking.

(define-record-type &comparator
  (raw-make-comparator checker equality compare hash)
  comparator?
  (checker checker)
  (equality equality)
  (compare compare)
  (hash hash))

;;; Syntax (must come first by R7RS rules)

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

;;; Primitive constructor and applicators

;; Safe version of make-comparator that handles #f arguments
(define (make-comparator checker equality compare hash)
  (raw-make-comparator
    (if checker checker (lambda (x) #t))
    (if equality equality (lambda (x y) (= 0 (compare x y))))
    (if compare compare (lambda (x y) (error "incomparable" x y)))
    (if hash hash (lambda (x) (error "unhashable" x)))))

;; Invoke the four procedures of a comparator
(define (comparator-type-check c x)
  ((type-check c) x))

(define (comparator-equal? c x y)
  ((equality c) x y))

(define (comparator-compare c x y)
  ((compare c) x y))

(define (comparator-hash c x)
  ((comparator c) x))

;;; Compare procedure constructors

(define (make-compare<? <)
  (lambda (x y)
    (cond
      ((< x y) -1)
      ((< y x) 1)
      (else 0))))

(define (make-compare>? >)
  (lambda (x y)
    (cond
      ((> x y) 1)
      ((> y x) -1)
      (else 0))))

(define (make-compare<=? <=)
  (lambda (x y)
    (let ((x<=y (<= x y))
          (y<=x (<= y x)))
      (cond
        ((and x<=y y<=x) 0)
        (x<=y -1)
        (else 1)))))

(define (make-compare>=? >=)
  (lambda (x y)
    (let ((x>=y (>= x y))
          (y>=x (>= y x)))
      (cond
        ((and x>=y y>=x) 0)
        (x>=y 1)
        (else -1)))))

(define (make-compare=/<? = <)
  (lambda (x y)
    (cond
      ((= x y) 0)
      ((< x y) 1)
      (else -1))))

(define (make-compare=/>? = >)
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

;; Boolean comparator
(define boolean-comparator
  (make-comparator boolean? boolean=? boolean-compare boolean-hash))

;; Character comparator
(define char-comparator
  (make-comparator character? char=?
    (make-compare=/< char=? char<?)
    char->integer))

(define char-ci-comparator
  (make-comparator character? char-ci=?
    (make-compare=/< char-ci=? char-ci<?)
    char->integer))

(define string-comparator
  (make-comparator string? string=?
    (make-compare=/< string=? string<?)
    string->integer))

(define string-ci-comparator
  (make-comparator string? string-ci=?
    (make-compare=/< string-ci=? string-ci<?)
    string->integer))

;; Numerical comparators

(define number-comparator
  (make-comparator number? = numeric-compare numeric-hash))

(define complex-comparator
  (make-comparator complex? = numeric-compare numeric-hash))

(define real-comparator
  (make-comparator real? = numeric-compare numeric-hash))

(define rational-comparator
  (make-comparator rational? = numeric-compare numeric-hash))
(define integer-comparator
  (make-comparator integer? = numeric-compare numeric-hash))

(define exact-integer-comparator
  (make-comparator exact-integer? = numeric-compare numeric-hash))

;;; Comparison procedures

(define =?
  (case-lambda
    ((x y) (=? default-comparator x y))
    ((c x y) ((comparator-equal? c) x y))))

(define not=?
  (case-lambda
    ((x y) (not=? default-comparator x y))
    ((c x y) (not ((comparator-equal? c) x y)))))

(define <?
  (case-lambda
    ((x y) (<? default-comparator x y))
    ((c x y) (eqv? -1 ((comparator-compare c) x y)))))

(define >=?
  (case-lambda
    ((x y) (>=? default-comparator x y))
    ((c x y) (not (eqv? -1 ((comparator-compare c) x y))))))

(define >?
  (case-lambda
    ((x y) (>? default-comparator x y))
    ((c x y) (eqv? 1 ((comparator-compare c) x y)))))

(define <=?
  (case-lambda
    ((x y) (<=? default-comparator x y))
    ((c x y) (not (eqv? 1 ((comparator-compare c) x y))))))

(define </<?
  (case-lambda
    ((x y z) (</<? default-comparator x y z))
    ((c x y) (and (<? c x y) (<? c y z)))))

(define </<=?
  (case-lambda
    ((x y z) (</<? default-comparator x y z))
    ((c x y) (and (<? c x y) (<=? c y z)))))

(define <=/<?
  (case-lambda
    ((x y z) (<=/<? default-comparator x y z))
    ((c x y) (and (<=? c x y) (<? c y z)))))

(define <=/<=?
  (case-lambda
    ((x y z) (<=/<=? default-comparator x y z))
    ((c x y) (and (<=? c x y) (<=? c y z)))))

(define >/>?
  (case-lambda
    ((x y z) (>/>? default-comparator x y z))
    ((c x y) (and (>? c x y) (>? c y z)))))

(define >/>=?
  (case-lambda
    ((x y z) (>/>? default-comparator x y z))
    ((c x y) (and (>? c x y) (>=? c y z)))))

(define >=/>?
  (case-lambda
    ((x y z) (>=/>? default-comparator x y z))
    ((c x y) (and (>=? c x y) (>? c y z)))))

(define >=/>=?
  (case-lambda
    ((x y z) (>=/>=? default-comparator x y z))
    ((c x y) (and (>=? c x y) (>=? c y z)))))

