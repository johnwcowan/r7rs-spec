;;;; Standard comparators and comparator constructors

;;; Standard atomic comparators

(define (boolean-comparison a b)
  (cond
    ((and a b) 0)
    (a 1)
    (b -1)
    (else 0)))

(define (boolean-hash obj) (if obj 1 0))

(define boolean-comparator
  (make-comparator boolean? boolean=? boolean-comparison boolean-hash))

(define char-comparison (make-comparison=/< char=? char<?))

(define (char-hash obj) (abs (char->integer obj)))

(define char-comparator
  (make-comparator char? char=? char-comparison char-hash))

(define char-ci-comparison (make-comparison=/< char-ci=? char-ci<?))

(define (char-ci-hash obj) (abs (char->integer (char-foldcase obj))))

(define char-ci-comparator
  (make-comparator char? char-ci=? char-ci-comparison char-ci-hash))

(define string-comparison (make-comparison=/< string=? string<?))

(define (string-hash obj)
  (make-vectorwise-hash char-hash string-length string-ref))

(define string-comparator
  (make-comparator string? string=? string-comparison string-hash))

(define string-ci-comparison (make-comparison=/< string-ci=? string-ci<?))

(define (string-ci-hash obj) (string-hash (string-foldcase obj)))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define symbol-comparison (make-comparison=/< symbol=? symbol<?))

(define (symbol-hash obj) (string-hash (symbol->string obj)))

(define symbol-comparator
  (make-comparator symbol? symbol=? symbol-comparison symbol-hash))

;; Comparison procedure for real numbers only
(define (real-comparison a b)
  (cond
    ((< a b) -1)
    ((> a b) 1)
    (else 0)))

(define (number-hash obj) (exact (abs obj)))

(define number-comparator
  (make-comparator number? = complex-comparison number-hash))

(define complex-comparator
  (make-comparator complex? = complex-comparison number-hash))

(define real-comparator
  (make-comparator real? = real-comparison number-hash))

(define rational-comparator
  (make-comparator rational? = real-comparison number-hash))

(define integer-comparator
  (make-comparator integer? = real-comparison number-hash))

(define exact-integer-comparator
  (make-comparator exact-integer? = real-comparison number-hash))

;;; Inexact real comparator

;; Test procedure for inexact reals
(define (inexact-real? obj) (and (number? obj) (inexact? obj) (real? obj)))

;; Return an appropriately rounded number
(define (rounded x symbol)
  (cond
    ((eq? symbol 'round) (round x))
    ((eq? symbol 'ceiling) (ceiling x))
    ((eq? symbol 'floor) (floor x))
    ((eq? symbol 'truncate) (truncate x))
    (else (error "invalid rounding specification" symbol))))

;; Return a number appropriately rounded to epsilon
(define (rounded-to x epsilon symbol)
  (rounded (/ x epsilon) symbol))

;; Returns result of comparing a NaN with a non-NaN
(define (nan-comparison nan-handling which)
  (cond
    ((eq? nan-handling 'error)
     (error "Attempt to compare NaN with non-NaN"))
    ((eq? nan-handling 'min)
     (if (eq? which 'a-nan) -1 1))
    ((eq? nan-handling 'max)
     (if (eq? which 'a-nan) 1 -1))
    (else
     (error "Invalid nan-handling specification"))))

(define (make-inexact-real-comparison epsilon rounding nan-handling)
  (lambda (a b)
    (let ((a-nan? (nan? a)) (b-nan? (nan? b)))
      (cond
        ((and a-nan? b-nan?) 0)
        (a-nan? (nan-comparison nan-handling 'a-nan))
        (b-nan? (nan-comparison nan-handling 'b-nan))
        (else (real-comparison
                (rounded-to a epsilon rounding)
                (rounded-to b epsilon rounding)))))))

;; Return 0 for NaN, number-hash otherwise
(define (inexact-real-hash obj)
  (if (nan? obj) 0 (number-hash obj)))

(define (make-inexact-real-comparator epsilon rounding nan-handling)
  (make-comparator
    inexact-real?
    #f
    (inexact-real-comparison epsilon rounding nan-handling)
    inexact-real-hash))

;;; Sequence comparator constructors and comparators
;;; The hash functions are based on Java's String.hash(), but
;;; modulo 2^20 instead of 2^32 in hopes of sticking to fixnums.

(define limit (expt 2 20))

;; Makes a comparison procedure that works listwise
(define (make-listwise-comparison comparison null? car cdr)
  (lambda (a b)
    (let ((a-null? (null? a)) (b-null? (null? b)))
      (cond
        ((and a-null? b-null?) 0)
        (a-null? -1)
        (b-null? 1)
        (else (let ((result (comparison (car a) (car b))))
          (if (= result 0) (comparison (cdr a) (cdr b)) result)))))))

;; Makes a hash function that works listwise
(define (make-listwise-hash hash null? car cdr)
  (lambda (obj)
    (let loop ((obj obj) (result 0))
      (if (null? obj)
        0
        (let* ((prod (modulo (* result 31) limit))
               (sum (modulo (+ prod (hash (car obj))))))
          (loop (cdr obj) sum))))))

;; Makes a comparison procedure that works vectorwise
(define (make-vectorwise-comparison comparison length ref)
  (lambda (a b)
    (let ((a-length (length a)) (b-length (length b)))
      (cond
        ((< a-length b-length) -1)
        ((> a-length b-length) 1)
        (else
          (call/cc
            (lambda (return)
              (let loop ((index 0))
                (let ((result (comparison (ref a index) (ref b index))))
                  (if (= result 0)
                    (loop (+ index 1))
                    result))))))))))

;; Makes a hash function that works vectorwise
(define (make-vectorwise-hash hash length ref)
  (lambda (obj)
    (let loop ((index (- (length obj) 1)) (result 0))
      (if (= index 0)
        result
        (let* ((prod (modulo (* result 31) limit))
               (sum (modulo (+ prod (hash (ref obj index))) limit)))
          (loop (- index 1) sum))))))


(define (make-listwise-comparator test comparator null? car cdr)
  (make-comparator
    test
    #f
    (make-listwise-comparison
      (comparator-comparison-procedure comparator) null? car cdr)
    (make-listwise-hash
      (comparator-hash-function comparator) null? car cdr)))

(define (make-vectorwise-comparator test comparator length ref)
  (make-comparator
    test
    #f
    (make-vectorwise-comparison
      (comparator-comparison-procedure comparator) length ref)
    (make-vectorwise-hash
      (comparator-hash-function comparator) length ref)))

(define (make-list-comparator comparator)
   (make-listwise-comparator
     (lambda (obj) (or (null? obj) (pair? obj)))
     comparator null? car cdr))

(define list-comparator (make-list-comparator default-comparator))

(define (make-vector-comparator comparator)
  (make-vectorwise-comparator vector? comparator vector-length vector-ref))

(define (make-bytevector-comparator comparator)
  (make-vectorwise-comparator
    bytevector? comparator bytevector-length bytevector-u8-ref))

(define bytevector-comparator (make-bytevector-comparator default-comparator))

;;; Pair comparator constructors

(define (make-car-comparator comparator)
  (make-comparator
    pair?
    #f
    (lambda (a b)
       (comparator-compare comparator (car a) (car b)))
    (lambda (obj) (comparator-hash-function comparator))))

(define (make-cdr-comparator comparator)
  (make-comparator
    pair?
    #f
    (lambda (a b)
       (comparator-compare comparator (cdr a) (cdr b)))
    (lambda (obj) (comparator-hash-function comparator obj))))

(define (make-pair-comparison car-comparator cdr-comparator)
  (lambda (a b)
    (let ((result (comparator-compare car-comparator (car a) (car b))))
      (if (= result 0)
        (comparator-compare cdr-comparator (cdr a) (cdr b))
        result))))

(define (make-pair-hash car-comparator cdr-comparator)
  (lambda (obj)
    (+
      (comparator-hash car-comparator (car obj))
      (comparator-hash cdr-comparator (cdr obj)))))

(define (make-pair-comparator car-comparator cdr-comparator)
  (make-comparator
    pair?
    #f
    (make-pair-comparison car-comparator cdr-comparator)
    (make-pair-hash car-comparator cdr-comparator)))

;; Compute type index for inexact list comparisons
(define (improper-list-type obj)
  (cond
    ((null? obj) 0)
    ((pair? obj) 1)
    (else 2)))

(define (make-improper-list-comparison comparator)
  (let ((pair-comparison (make-pair-comparison comparator comparator)))
    (lambda (a b)
      (let* ((a-type (improper-list-type a))
            (b-type (improper-list-type b))
            (result (real-comparison a-type b-type)))
        (cond
           ((not (= result 0)) result)
           ((null? a) 0)
           ((pair? a) (pair-comparison a b))
           (else (comparison-compare comparator a b))))))

(define (make-improper-list-hash comparator)
  (lambda (obj)
    (cond
      ((null? obj) 0)
      ((pair? obj) (+ (comparator-hash comparator (car obj))
                      (comparator-hash comparator (cdr obj))))
      (else (comparator-hash comparator obj)))))

(define (make-improper-list-comparator comparator)
  (make-comparator
    #f
    #f
    (make-improper-list-comparison comparator)
    (make-improper-list-hash comparator)))

;;; Wrapped equality predicates
;;; These comparators don't have comparison functions.

(define eq-comparator
  (make-comparator
    #f
    eq?
    #f
    (comparator-hash-function default-comparator)))

(define eqv-comparator
  (make-comparator
    #f
    eqv?
    #f
    (comparator-hash-function default-comparator)))

(define equal-comparator
  (make-comparator
    #f
    equal?
    #f
    (comparator-hash-function default-comparator)))

