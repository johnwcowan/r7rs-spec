;;; Ilist comparator constructor

(define (make-ilist-comparator comparator)
   (make-listwise-comparator
     (lambda (obj) (or (null? obj) (ipair? obj)))
     comparator null? icar icdr))

(define ilist-comparator (make-ilist-comparator default-comparator))

;;; Ipair comparator constructors

(define (make-icar-comparator comparator)
  (make-comparator
    ipair?
    #t
    (lambda (a b)
       (comparator-compare comparator (icar a) (icar b)))
    (lambda (obj) (comparator-hash-function comparator))))

(define (make-icdr-comparator comparator)
  (make-comparator
    ipair?
    #t
    (lambda (a b)
       (comparator-compare comparator (icdr a) (icdr b)))
    (lambda (obj) (comparator-hash comparator obj))))

(define (make-ipair-comparison icar-comparator icdr-comparator)
  (lambda (a b)
    (let ((result (comparator-compare icar-comparator (icar a) (icar b))))
      (if (= result 0)
        (comparator-compare icdr-comparator (icdr a) (icdr b))
        result))))

(define ipair-comparison
  (make-ipair-comparison default-comparator default-comparator))

(define (make-ipair-hash icar-comparator icdr-comparator)
  (lambda (obj)
    (+
      (comparator-hash icar-comparator (icar obj))
      (comparator-hash icdr-comparator (icdr obj)))))

(define (make-ipair-comparator icar-comparator icdr-comparator)
  (make-comparator
    ipair?
    #t
    (make-ipair-comparison icar-comparator icdr-comparator)
    (make-ipair-hash icar-comparator icdr-comparator)))

(define ipair-comparator
  (make-ipair-comparator default-comparator default-comparator))

(define ipair-hash (comparator-hash-function ipair-comparator))

;; Compute type index for inexact ilist comparisons
(define (improper-ilist-type obj)
  (cond
    ((null? obj) 0)
    ((ipair? obj) 1)
    (else 2)))

;;; Improper ilists comparison

(define (real-comparison a b)
  (cond
    ((< a b) -1)
    ((> a b) 1)
    (else 0)))

(define (make-improper-ilist-comparison comparator)
  (let ((ipair-comparison (make-ipair-comparison comparator comparator)))
    (lambda (a b)
      (let* ((a-type (improper-ilist-type a))
            (b-type (improper-ilist-type b))
            (result (real-comparison a-type b-type)))
        (cond
           ((not (= result 0)) result)
           ((null? a) 0)
           ((ipair? a) (ipair-comparison a b))
           (else (comparator-compare comparator a b)))))))

(define (make-improper-ilist-hash comparator)
  (lambda (obj)
    (cond
      ((null? obj) 0)
      ((ipair? obj) (+ (comparator-hash comparator (icar obj))
                      (comparator-hash comparator (icdr obj))))
      (else (comparator-hash comparator obj)))))

(define (make-improper-ilist-comparator comparator)
  (make-comparator
    #t
    #t
    (make-improper-ilist-comparison comparator)
    (make-improper-ilist-hash comparator)))

;;; Register ilist-comparator with SRFI 114 sample implementation.
;;; If you don't use the sample implementation of SRFI 114, do something else.

(comparator-register-default! ilist-comparator)
