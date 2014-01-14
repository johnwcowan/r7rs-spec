;;; The default comparator

;; Return ordinal for standard types: null sorts before pairs, which sort
;; before booleans, etc.  Implementations should extend this.
(define (object-type obj)
  (cond
    ((null? obj) 0)
    ((pair? obj) 1)
    ((boolean? obj) 2)
    ((char? obj) 3)
    ((string? obj) 4)
    ((symbol? obj) 5)
    ((number? obj) 6)
    ((vector? obj) 7)
    ((bytevector? obj) 8)
    ; Add more here
    (else 32767)))

(define (dispatch-comparison type a b)
  (case type
    ((0) 0) ; All empty lists are equal
    ((1) (pair-comparison a b))
    ((2) (boolean-comparison a b))
    ((3) (char-comparison a b))
    ((4) (string-comparison a b))
    ((5) (symbol-comparison a b))
    ((6) (complex-comparison a b))
    ((7) (vector-comparison a b))
    ((8) (bytevector-comparison a b))
    ; Add more here
    ((32767) 0))) ; All unknown objects are equal

(define (default-hash-function obj)
  (case (object-type obj)
    ((0) 0)
    ((1) (pair-hash obj))
    ((2) (boolean-hash obj))
    ((3) (char-hash obj))
    ((4) (string-hash obj))
    ((5) (symbol-hash obj))
    ((6) (number-hash obj))
    ((7) (vector-hash obj))
    ((8) (bytevector-hash obj))
    ; Add more here
    ((32767) 0))) ; All unknown objects hash the same

(define (default-comparison a b)
  (let ((a-type (object-type a))
        (b-type (object-type b)))
    (cond
      ((< a-type b-type) -1)
      ((> a-type b-type) 1)
      (else (dispatch-comparison a-type a b)))))

(define default-comparator
  (make-comparator
    #t
    #t
    default-comparison
    default-hash-function))

