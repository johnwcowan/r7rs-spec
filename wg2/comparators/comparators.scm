;;; Standard atomic comparators

(define (boolean-compare a b)
  (cond
    ((and a b) 0)
    (a 1)
    (b -1)
    (else 0)))

(define (boolean-hash obj) (if obj 1 0))

(define boolean-comparator
  (make-comparator boolean? boolean=? boolean-compare boolean-hash))

(define char-compare (make-comparison=/< char=? char<?))

(define (char-hash obj) (abs (char->integer obj)))

(define char-comparator
  (make-comparator char? char=? char-compare char-hash))

(define char-ci-compare (make-comparison=/< char-ci=? char-ci<?))

(define (char-ci-hash obj) (abs (char->integer (char-foldcase obj))))

(define char-ci-comparator
  (make-comparator char? char-ci=? char-ci-compare char-ci-hash))

(define string-compare (make-comparison=/< string=? string<?))

(define (string-hash obj) 'FIXME)

(define string-comparator
  (make-comparator string? string=? string-compare string-hash))

(define string-ci-compare (make-comparison=/< string-ci=? string-ci<?))

(define (string-ci-hash obj) (string-hash (string-foldcase obj)))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci-compare string-ci-hash))

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define symbol-compare (make-comparison=/< symbol=? symbol<?))

(define (symbol-hash obj) (string-hash (symbol->string obj)))

(define symbol-comparator
  (make-comparator symbol? symbol=? symbol-compare symbol-hash))

(define (number-hash obj) (exact (abs obj)))

(define number-comparator
  (make-comparator number? = complex-compare number-hash))

(define complex-comparator
  (make-comparator complex? = complex-compare number-hash))

(define real-comparator
  (make-comparator real? = real-compare number-hash))

(define rational-comparator
  (make-comparator rational? = real-compare number-hash))

(define integer-comparator
  (make-comparator integer? = real-compare number-hash))

(define exact-integer-comparator
  (make-comparator exact-integer? = real-compare number-hash))

;;; Sequence comparator constructors and comparators
;;; The hash functions are based on Java's String.hash(), but
;;; modulo 2^20 instead of 2^32 in hopes of sticking to fixnums.

(define limit (expt 2 20))

;; Makes a comparison procedure that works listwise
(define (make-listwise-compare comparison null? car cdr)
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
(define (make-vectorwise-compare comparison length ref)
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
    (make-listwise-compare
      (comparator-comparison-procedure comparator) null? car cdr)
    (make-listwise-hash
      (comparator-hash-function comparator) null? car cdr)))

(define (make-vectorwise-comparator test comparator length ref)
  (make-comparator
    test
    #f
    (make-vectorwise-compare
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
       (comparator-comparison-procedure comparator (car a) (car b)))
    (lambda (obj) (comparator-hash-function comparator))))

(define (make-cdr-comparator comparator)
  (make-comparator
    pair?
    #f
    (lambda (a b)
       (comparator-comparison-procedure comparator (cdr a) (cdr b)))
    (lambda (obj) (comparator-hash-function comparator obj))))

