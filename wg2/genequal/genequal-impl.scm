;;;; Generalized equality

;; Return if x and y are equal in the sense of the comparators in clist.
(define (generalized-equal? x y . initial-clist)
  (if (eqv? x y)
    #t
    (let loop ((clist initial-clist))
      (if (not (null? clist))
        (case ((car clist) x y initial-clist)
          ((#t) #t)
          ((#f) #f)
          ((pass) (loop (cdr clist)))
          (else (error "generalized-equal?: comparator returned bad result"
                         (car clist))))
        (let ((l (list-comparator x y initial-clist))
              (s (string-comparator x y initial-clist))
              (v (vector-comparator x y initial-clist))
              (b (bytevector-comparator x y initial-clist)))
           (cond
             ((boolean? l) l)
             ((boolean? s) s)
             ((boolean? v) v)
             ((boolean? b) b)
             (else #f)))))))

;; Create a comparator based on a type predicate and an equivalence predicate
(define (make-atomic-comparator type? equivalent?)
  (lambda (x y clist)
    (if (or (not (type? x)) (not (type? y)))
        'pass
        (equivalent? x y))))

;; Create a curried equality predicate
(define (make-specific-equality . comparator-list)
  (lambda (x y) (apply generalized-equal? x y comparator-list)))

;;; Standard comparators

(define numeric-comparator (make-atomic-comparator number? =))

(define (list-comparator x y clist)
  (let loop ((x x) (y y))
    (cond
      ((and (null? x) (null? y)) #t)
      ((null? x) #f)
      ((null? y) #f)
      ((not (pair? x)) 'pass)
      ((not (pair? y)) 'pass)
      ((apply generalized-equal? (car x) (car y) clist) (loop (cdr x) (cdr y)))
      (else #f))))

(define (vector-comparator x y clist)
  (cond
    ((or (not (vector? x)) (not (vector? y)))
     'pass)
    ((not (= (vector-length x) (vector-length y)))
     #f)
    (else
      (count-up (return #t) (i 0 (vector-length x))
        (if (not (apply generalized-equal?
                   (vector-ref x i)
                   (vector-ref y i)
                   clist))
           (return #f))))))

;; Change this to (define (bytevector-comparator x y clist) 'pass)
;; if you don't have bytevectors
(define (bytevector-comparator x y clist)
  (cond
    ((or (not (bytevector? x)) (not (bytevector? y)))
     'pass)
    ((not (= (bytevector-length x) (bytevector-length y)))
     #f)
    (else
      (count-up (return #t) (i 0 (bytevector-length x))
        (if (not (= (bytevector-u8-ref x i) (bytevector-u8-ref y i)))
           (return #f))))))

(define string-comparator (make-atomic-comparator string? string=?))

(define string-ci-comparator (make-atomic-comparator string? string-ci=?))

(define char-ci-comparator (make-atomic-comparator char? char-ci=?))
