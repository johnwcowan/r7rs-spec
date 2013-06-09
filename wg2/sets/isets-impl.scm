;;;; Implementation of integer sets for SRFI xxx

;;; This implementation is written in portable R5RS plus SRFIs 9 and 23
;;; and bytevectors (we use the R7RS version here).  An implementation
;;; of all these SRFIs on top of R6RS is available.

;; Integer sets are records with two fields, a limit and a bytevector.
;; If the nth element of the bytevector is 1, the integer-set includes n.
;; Otherwise it is 0.

;; We don't bother packing bits into the bytevector.  We keep the limit
;; separate and don't assume we can retrieve it from the bytevector.
;; This makes it easy to change the implementation to do bit packing.
(define-record-type &integer-set
  (raw-make-integer-set limit bv)
  integer-set?
  (limit limit)
  (bv bv))

;;; These procedures directly depend on the underlying representation.

;; Create an empty integer-set with a specified limit.
(define (make-integer-set limit)
  (raw-make-integer-set limit (make-bytevector limit 0)))

;; Integer-set-specific: create a full integer-set with a specified limit.
(define (make-universal-integer-set limit)
  (raw-make-integer-set limit (make-bytevector limit 1)))

;; Return the number of elements in an integer-set.
(define (integer-set-size is)
  (iset-check is)
  (let ((count 0) (bv (bv is)))
    (count-up (i 0 (limit is))
      (if (= 1 (bytevector-u8-ref bv i))
        (set! count (+ count 1))))
    count))

;; Return #t if an element e is a member of the integer-set is.
(define (integer-set-member? is n)
  (if (and (number? n) (exact? n) (>= n 0) (< n (limit (iset-check is))))
    (= 1 (bytevector-u8-ref (bv is) n))
    #f))

;; Add a new element.
(define (integer-set-add! is e)
  (bytevector-u8-set! (bv (iset-check-limit is e)) e 1))

;; Remove an element.  If the element was present, return #t, otherwise #f.
(define (integer-set-delete! is e)
  (let ((old (bytevector-u8-ref (bv (iset-check-limit is e)) e)))
    (bytevector-u8-set! (bv (iset-check-limit is e)) e 0)
    (= old 1)))

;; Map a function over an integer-set, discarding the results.
(define (integer-set-for-each proc is)
  (let ((bv (bv (iset-check is))))
    (count-up (i 0 (limit is))
      (if (= 1 (bytevector-u8-ref bv i))
        (proc i)))))

;; Integer-set fold.  Returns nil if the integer-set is empty.
(define (integer-set-fold proc nil is)
  (iset-check is)
  (let ((bv (bv is))
        (r nil))
    (count-up (i 0 (limit is))
      (if (= 1 (bytevector-u8-ref bv i))
        (set! r (proc i r))))
    r))

;; Integer-set-specific: fast copy
(define (integer-set-copy is)
  (iset-check is)
  (raw-make-integer-set (limit is) (bytevector-copy (bv is))))

(define (integer-set-empty-copy is)
  (iset-check is)
  (make-integer-set (limit is)))

;; Integer-set-specific: complement an integer-set
(define (integer-set-complement! is)
  (iset-check is)
  (let ((bv (bv is)))
    (count-up (i 0 (limit is))
      (bytevector-u8-set! bv i (- 1 (bytevector-u8-ref bv i))))
    is))

;; Integer-set-specific: return smallest value of is
(define (integer-set-min is)
  (iset-check is)
  (let ((bv (bv is)))
    (count-up (return #f) (i 0 (limit is))
      (if (= 1 (bytevector-u8-ref bv i))
         (return i)))))

;; Integer-set-specific: return and delete smallest value of is
(define (integer-set-delete-min! is)
  (iset-check is)
  (let ((bv (bv is)))
    (count-up (return #f) (i 0 (limit is))
      (if (= 1 (bytevector-u8-ref bv i))
         (begin (bytevector-u8-set! bv i 0) (return i))))))

;; Integer-set-specific: return largest value of is
(define (integer-set-max is)
  (iset-check is)
  (let ((bv (bv is)))
    (count-down (return #f) (i 0 (limit is))
      (if (= 1 (bytevector-u8-ref bv i))
         (return i)))))

;; Integer-set-specific: return and delete largest value of is
(define (integer-set-delete-max! is)
  (iset-check is)
  (let ((bv (bv is)))
    (count-down (return #f) (i 0 (limit is))
      (if (= 1 (bytevector-u8-ref bv i))
         (begin (bytevector-u8-set! bv i 0) (return i))))))

;;; These procedures do not directly depend on the underlying representation.

;; Internal: signal an error if obj is not an integer-set.
(define (iset-check obj)
  (if (integer-set? obj) obj (error "Integer-set expected" obj)))

;; Internal: signal an error if n is out of range for is.
(define (iset-check-limit is n)
  (if (and (number? n) (exact? n) (>= n 0) (< n (limit (iset-check is))))
    is
    (error "Integer-set value out of range" is n)))

;; Internal: check that the limits of is1 and is2 are the same.
(define (iset-check-limits is1 is2)
  (if (= (limit is1) (limit is2))
    #t
    (error "Integer-sets have different limits" is1 is2)))

;; Create an integer-set with a specified limit and populate it.
(define (integer-set limit . elements)
  (let ((t (make-integer-set limit)))
    (for-each (lambda (e) (integer-set-add! t e)) elements)
    t))

;; Return a copy of an integer-set, using the same limit.
;; Map a function over an integer-set.
(define (integer-set-map limit proc is)
  (let ((t (make-integer-set limit)))
    (integer-set-for-each (lambda (e) (integer-set-add! t (proc e))) (iset-check is))
    t))

;; Return a list of the integer-set members.
(define (integer-set->list is)
  (reverse (integer-set-fold cons '() (iset-check is))))

;; Create an integer-set from a limit and a list.
(define (list->integer-set limit list)
  (let ((t (make-integer-set limit)))
    (for-each (lambda (e) (integer-set-add! t e)) list)
    t))

;; Return #t if all sets are equal, #f otherwise.
(define (integer-set=? is . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-integer-set=? (iset-check is) (car sets))
     (apply integer-set=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of integer-set=?.
(define (dyadic-integer-set=? is1 is2)
  (iset-check-limits is1 is2)
  (and (dyadic-integer-set<=? is1 is2) (dyadic-integer-set>=? is1 is2)))

;; Return #t if each integer-set is a proper subset of the following integer-set.
(define (integer-set<? is . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-integer-set<? (iset-check is) (car sets))
     (apply integer-set<? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of integer-set<?.
(define (dyadic-integer-set<? is1 is2)
  (iset-check-limits is1 is2)
  (not (dyadic-integer-set>=? is1 is2)))

;; Return #t if each integer-set is a proper superset of the following integer-set.
(define (integer-set>? is . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-integer-set>? (iset-check is) (car sets))
     (apply integer-set>? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of integer-set>?.
(define (dyadic-integer-set>? is1 is2)
  (iset-check-limits is1 is2)
  (not (dyadic-integer-set<=? is1 is2)))

;; Return #t if each integer-set is a subset (proper or improper) of the following integer-set.
(define (integer-set<=? is . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-integer-set<=? (iset-check is) (car sets))
     (apply integer-set<=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of integer-set<=?.
(define (dyadic-integer-set<=? is1 is2)
  (iset-check-limits is1 is2)
  (call/cc
    (lambda (return)
      (integer-set-for-each
        (lambda (e)
          (if (integer-set-member? is2 e) #t (return #f)))
        is1)
      #t)))

(define (integer-set>=? is . sets)
;; Return #t if each integer-set is a superset (proper or improper) of the following integer-set.
  (cond
    ((null? sets)
     #t)
    ((dyadic-integer-set>=? (iset-check is) (car sets))
     (apply integer-set=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of integer-set>=?.
(define (dyadic-integer-set>=? is1 is2)
  (iset-check-limits is1 is2)
  (dyadic-integer-set<=? is2 is1))

;; Return the union of all sets.
(define (integer-set-union is . sets)
  (apply integer-set-union! (integer-set-copy (iset-check is)) sets))

;; Return the union of all sets, destroying the first.
(define (integer-set-union! is . sets)
  (iset-check is)
  (for-each
    (lambda (integer-set)
      (iset-check-limits is integer-set)
      (integer-set-for-each (lambda (e) (integer-set-add! is e)) (iset-check integer-set)))
    sets)
  is)

;; Return the intersection of all sets.
(define (integer-set-intersection is . sets)
  (apply integer-set-intersection! (integer-set-copy (iset-check is)) sets))

;; Return the intersection of all sets, destroying the first.
(define (integer-set-intersection! is . sets)
  (iset-check is)
  (for-each
    (lambda (integer-set) (dyadic-integer-set-intersection! is (iset-check integer-set)))
    sets)
  is)

;; Internal: return the intersection of two sets, destroying the first.
(define (dyadic-integer-set-intersection! is1 is2)
  (iset-check-limits is1 is2)
  (integer-set-for-each
    (lambda (e)
      (if (integer-set-member? is2 e) #t (integer-set-delete! is1 e)))
    is1)
  is1)

;; Return the asymmetric difference of is and the union of the other sets.
(define (integer-set-difference is . sets)
  (apply integer-set-difference! (integer-set-copy (iset-check is)) sets))

;; Asymmetric difference, destroying is.
(define (integer-set-difference! is . sets)
  (iset-check is)
  (for-each
    (lambda (integer-set)
      (iset-check-limits is integer-set)
      (integer-set-for-each (lambda (e) (integer-set-delete! is e)) (iset-check integer-set)))
    sets)
  is)

;; Exclusive or (symmetric difference) of two sets.
(define (integer-set-xor is1 is2)
  (iset-check-limits is1 is2)
  (let ((t (integer-set-copy (iset-check is1))))
    (integer-set-xor! t (iset-check is2))))

;; Exclusive or, destroying the first integer-set.
;; Not the most efficient implementation....
(define (integer-set-xor! is1 is2)
  (iset-check-limits is1 is2)
  (let ((int (integer-set-intersection is1 is2)))
    (integer-set-difference! (integer-set-union! is1 is2) int)))

;; Integer-set-specific: return the complement of an integer-set
(define (integer-set-complement is)
  (integer-set-complement! (integer-set-copy (iset-check is))))

;; Construct an integer set by unfolding
(define (integer-set-unfold limit continue? mapper successor seed)
  (let loop ((s (make-integer-set limit))
             (seed seed))
    (if (continue? seed)
      (let ((r (mapper seed)))
        (integer-set-add! s r)
        (loop s (successor seed)))
      s)))

;; Return integer-set that contains elements that satisfy pred
(define (integer-set-filter pred is)
  (let ((t (integer-set-empty-copy is)))
    (integer-set-for-each
      (lambda (x) (if (pred x) (integer-set-add! t x)))
      is)
    t))

;; Return integer-set that contains elements that don't satisfy pred
(define (integer-set-remove pred is)
  (let ((t (integer-set-empty-copy is)))
    (integer-set-for-each
      (lambda (x) (if (not (pred x)) (integer-set-add! t x)))
      is)
    t))

;; Partition integer-set into elements that do and don't satisfy pred
(define (integer-set-partition pred is)
  (let ((yes (integer-set-empty-copy is))
        (no (integer-set-empty-copy is)))
    (integer-set-for-each
      (lambda (x)
        (if (pred x) (integer-set-add! yes x) (integer-set-add! no x)))
      is)
    (values yes no)))

;; Count matching elements of integer-set
(define (integer-set-count pred is)
  (let ((n 0))
    (integer-set-for-each
      (lambda (x)
        (if (pred x) (set! n (+ n 1))))
      is)
    n))

;; Return #t if any element of is satisfies pred
(define (integer-set-any? pred is)
  (call/cc
    (lambda (return)
      (integer-set-for-each
        (lambda (e) (if (pred e) (return #t)))
        is)
      #f)))

;; Return #t if every element of is satisfies pred
(define (integer-set-every? pred is)
  (call/cc
    (lambda (return)
      (integer-set-for-each
        (lambda (e) (if (not (pred e)) (return #f)))
        is)
      #t)))

;; Return an arbitrary element of is that satisfies pred,
;; or invoke failure if none
(define (integer-set-find pred is failure)
  (call/cc
    (lambda (return)
      (integer-set-for-each
        (lambda (e) (if (pred e) (return e)))
        is)
      (failure))))

;; Internal: print the contents of an integer-set, for debugging.
(define (print-integer-set is out)
  (iset-check is)
  (display "#<integer-set(" out)
  (display (limit is) out)
  (display ") " out)
  (integer-set-for-each (lambda (e) (display e out) (display #\space out)) is)
  (display ">" out))
