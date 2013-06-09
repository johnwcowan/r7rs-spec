;;;; Implementation of bags for SRFI xxx

;;; This implementation is written in portable R5RS plus SRFIs 9, 23, and
;;; 69.  An implementation of all these SRFIs on top of R6RS is available.
;;; This file is mostly cloned from bags.scm.

;; Bags are records with two fields, an equality predicate and a hash
;; table.  The hash table maps each bag member to the number of times
;; it appears in the bag.  We keep the equality predicate separate and
;; don't assume we can retrieve it from the hash table.  This makes it
;; easy to layer this SRFI over less powerful hash table systems.
(define-record-type &bag
  (raw-make-bag bag-equality bag-dict)
  bag?
  (bag-equality bag-equality)
  (bag-dict bag-dict))

;;; These procedures directly depend on the underlying representation.

;; Create an empty bag with a specified equality predicate.
(define (make-bag bag-equality)
  (raw-make-bag bag-equality (make-hash-table bag-equality)))

;; Return the number of elements in a bag.
;; Note that this is not the number of elements in the hash table.
(define (bag-size b)
  (hash-table-fold
    (bag-dict (bag-check b))
    (lambda (k v accum) (+ v accum))
    0))

;; Return #t if an element e is a member of a bag b.
(define (bag-member? b e)
  (hash-table-ref/default
    (bag-dict (bag-check b))
    e
    #f))

;; Add a new element.
(define (bag-add! b e)
  (bag-increment! b e 1))

;; Remove an element.  If the element was present, return #t, otherwise #f.
(define (bag-delete! b e)
  (bag-decrement! b e 1))

;; Map a function over a bag, discarding the results.
(define (bag-for-each proc b)
  (hash-table-walk
    (bag-dict (bag-check b))
    (lambda (k v) (each-element proc k v))))

;; Map a function over unique elements of a bag, discarding the results.
(define (bag-for-each-unique proc b)
  (hash-table-walk
    (bag-dict (bag-check b))
    (lambda (k v) (proc k))))

;; Internal:  walk each identical element
(define (each-element proc e n)
  (if (= n 0)
    #t
    (begin
      (proc e)
      (each-element proc e (- n 1)))))

;; Bag fold.  Returns nil if the bag is empty.
(define (bag-fold proc nil b)
  (let ((r nil))
    (hash-table-walk
      (bag-dict (bag-check b))
      (lambda (k v)
        (count-up (i 0 v)
          (set! r (proc k r)))))
    r))

;; Bag fold over unique elements.  Returns nil if the bag is empty.
;; Proc gets three arguments (element, count, accumulator).
(define (bag-fold-unique proc nil b)
  (let ((r nil))
    (hash-table-walk
      (bag-dict (bag-check b))
      (lambda (k v) (set! r (proc k v r))))
    r))

;; Bag-specific: Count the number of times element appears in a bag.
(define (bag-element-count b e)
  (hash-table-ref/default (bag-dict (bag-check b)) e 0))

;; Bag-specific:  Increment the number of times element appears in a bag.
(define (bag-increment! b e n)
  (bag-decrement! b e (- n)))

;; Bag-specific: Decrement the number of times element appears in a bag
;; (but not below zero).  Return #f if element not present enough times.
(define (bag-decrement! b e n)
  (let* ((old (bag-element-count b e))
         (new (- old n))
         (dict (bag-dict b)))
    (if (<= new 0)
      (hash-table-delete! dict e)
      (hash-table-set! dict e new))
    (>= new 0)))

;; Returns the member of a set which is the same (in the sense of the
;; equality predicate) as the argument.
;; Not part of the SRFI
#;(define (bag-value b e)
  (let loop ((keys (hash-table-keys (bag-dict (bag-check b)))))
    (cond
      ((null? keys)
       e)
      (((bag-equality b) (car keys) e)
       (car keys))
      (else
       (loop (cdr keys))))))

;; Return an empty copy of a bag, using the same equality predicate.
(define (bag-empty-copy b)
  (make-bag (bag-equality (bag-check b))))

;;; These procedures do not directly depend on the underlying representation.

;; Internal: signal an error if obj is not a bag.
(define (bag-check obj)
  (if (bag? obj) obj (error "Bag expected" obj)))

;; Internal: signal an error if b1 and b2 have different equality predicates.
(define (bag-check-equalities b1 b2)
  (if (eq? (bag-equality b1) (bag-equality b2))
    #t
    (error "Bag equality predicate discrepancy" b1 b2)))

;; Return a copy of a bag, using the same equality predicate.
(define (bag-copy b)
  (let ((t (bag-empty-copy b)))
    (bag-for-each (lambda (e) (bag-add! t e)) b)
    t))

;; Create a bag with a specified equality predicate and populate it.
(define (bag bag-equality . elements)
  (let ((t (make-bag bag-equality)))
    (for-each (lambda (e) (bag-add! t e)) elements)
    t))

;; Map a function over a bag.
(define (bag-map bag-equality proc b)
  (let ((t (make-bag bag-equality)))
    (bag-for-each (lambda (e) (bag-add! t (proc e))) (bag-check b))
    t))

;; Return a list of the bag members.
(define (bag->list b)
  (bag-fold cons '() (bag-check b)))

;; Create a bag from an equality predicate and a list.
(define (list->bag = list)
  (let ((t (make-bag =)))
    (for-each (lambda (e) (bag-add! t e)) list)
    t))

;; Return #t if all bags are equal, #f otherwise.
(define (bag=? b . bags)
  (cond
    ((null? bags)
     #t)
    ((dyadic-bag=? (bag-check b) (car bags))
     (apply bag=? (car bags) (cdr bags)))
    (else
     #f)))

;; Internal: dyadic version of bag=?.
(define (dyadic-bag=? b1 b2)
  (bag-check-equalities b1 b2)
  (and (dyadic-bag<=? b1 b2) (dyadic-bag>=? b1 b2)))

;; Return #t if each bag is a proper subset of the following bag.
(define (bag<? b . bags)
  (cond
    ((null? bags)
     #t)
    ((dyadic-bag<? (bag-check b) (car bags))
     (apply bag<? (car bags) (cdr bags)))
    (else
     #f)))

;; Internal: dyadic version of bag<?.
(define (dyadic-bag<? b1 b2)
  (bag-check-equalities b1 b2)
  (not (dyadic-bag>=? b1 b2)))

;; Return #t if each bag is a proper superset of the following bag.
(define (bag>? b . bags)
  (cond
    ((null? bags)
     #t)
    ((dyadic-bag>? (bag-check b) (car bags))
     (apply bag>? (car bags) (cdr bags)))
    (else
     #f)))

;; Internal: dyadic version of bag>?.
(define (dyadic-bag>? b1 b2)
  (bag-check-equalities b1 b2)
  (not (dyadic-bag<=? b1 b2)))

;; Return #t if each bag is a subset (proper or improper) of the following bag.
(define (bag<=? b . bags)
  (cond
    ((null? bags)
     #t)
    ((dyadic-bag<=? (bag-check b) (car bags))
     (apply bag<=? (car bags) (cdr bags)))
    (else
     #f)))

;; Internal: dyadic version of bag<=?.
(define (dyadic-bag<=? b1 b2)
  (bag-check-equalities b1 b2)
  (call/cc
    (lambda (return)
      (bag-for-each
        (lambda (e)
          (if (and (bag-member? b2 e)
                   (<= (bag-element-count b1 e) (bag-element-count b2 e)))
            #t
            (return #f)))
        b1)
      #t)))

(define (bag>=? b . bags)
;; Return #t if each bag is a superset (proper or improper) of the following bag.
  (cond
    ((null? bags)
     #t)
    ((dyadic-bag>=? (bag-check b) (car bags))
     (apply bag=? (car bags) (cdr bags)))
    (else
     #f)))

;; Internal: dyadic version of bag>=?.
(define (dyadic-bag>=? b1 b2)
  (bag-check-equalities b1 b2)
  (dyadic-bag<=? b2 b1))

;; Return the union of all bags.
(define (bag-union b . bags)
  (apply bag-union! (bag-copy (bag-check b)) bags))

;; Return the union of all bags, destroying the first.
(define (bag-union! b . bags)
  (bag-check b)
  (for-each
    (lambda (bag)
      (bag-check-equalities b bag)
      (bag-for-each (lambda (e) (bag-add! b e)) (bag-check bag)))
    bags)
  b)

;; Return the intersection of all bags.
(define (bag-intersection b . bags)
  (apply bag-intersection! (bag-copy (bag-check b)) bags))

;; Return the intersection of all bags, destroying the first.
(define (bag-intersection! b . bags)
  (bag-check b)
  (for-each
    (lambda (bag) (dyadic-bag-intersection! b (bag-check bag)))
    bags)
  b)

;; Internal: return the intersection of two bags, destroying the first.
(define (dyadic-bag-intersection! b1 b2)
  (bag-check-equalities b1 b2)
  (bag-for-each
    (lambda (e)
      (if (bag-member? b2 e) #t (bag-delete! b1 e)))
    b1)
  b1)

;; Return the asymmetric difference of b and the union of the other bags.
(define (bag-difference b . bags)
  (apply bag-difference! (bag-copy (bag-check b)) bags))

;; Asymmetric difference, destroying b.
(define (bag-difference! b . bags)
  (bag-check b)
  (for-each
    (lambda (bag)
      (bag-check-equalities b bag)
      (bag-for-each (lambda (e) (bag-delete! b e)) (bag-check bag)))
    bags)
  b)

;; Construct a set by unfolding
(define (bag-unfold equivalence continue? mapper successor seed)
  (let loop ((s (make-bag equivalence))
             (seed seed))
    (if (continue? seed)
      (let ((r (mapper seed)))
        (bag-add! s r)
        (loop s (successor seed)))
      s)))

;; Return bag that contains elements that satisfy pred
(define (bag-filter pred b)
  (let ((t (bag-empty-copy b)))
    (bag-for-each
      (lambda (x) (if (pred x) (bag-add! t x)))
      b)
    t))

;; Return bag that contains elements that don't satisfy pred
(define (bag-remove pred b)
  (let ((t (bag-empty-copy b)))
    (bag-for-each
      (lambda (x) (if (not (pred x)) (bag-add! t x)))
      b)
    t))

;; Partition bag into elements that do and don't satisfy pred
(define (bag-partition pred b)
  (let ((yes (bag-empty-copy b))
        (no (bag-empty-copy b)))
    (bag-for-each
      (lambda (x)
        (if (pred x) (bag-add! yes x) (bag-add! no x)))
      b)
    (values yes no)))

;; Count matching elements of bag
(define (bag-count pred b)
  (let ((n 0))
    (bag-for-each
      (lambda (x)
        (if (pred x) (set! n (+ n 1))))
      b)
    n))

;; Return #t if any element of b satisfies pred
(define (bag-any? pred b)
  (call/cc
    (lambda (return)
      (bag-for-each
        (lambda (e) (if (pred e) (return #t)))
        b)
      #f)))

;; Return #t if every element of b satisfies pred
(define (bag-every? pred b)
  (call/cc
    (lambda (return)
      (bag-for-each
        (lambda (e) (if (not (pred e)) (return #f)))
        b)
      #t)))

;; Return an arbitrary element of b that satisfies pred,
;; or invoke failure if none
(define (bag-find pred b failure)
  (call/cc
    (lambda (return)
      (bag-for-each
        (lambda (e) (if (pred e) (return e)))
        b)
      (failure))))

;; Convert a bag to a set
(define (bag->set b)
  (let ((s (make-set (bag-equality b))))
    (bag-for-each-unique (lambda (x) (set-add! s x)) b)
    s))

;; Convert a set to a bag
(define (set->bag s)
  (let ((b (make-bag (set-equality s))))
    (set-for-each (lambda (x) (bag-add! b x)) s)
    b))

;; Internal: print the contents of a bag, for debugging.
(define (print-bag b out)
  (bag-check b)
  (display "#<bag " out)
  (bag-for-each (lambda (e) (display e out) (display #\space out)) b)
  (display ">" out))

