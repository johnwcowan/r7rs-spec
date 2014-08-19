;;;; Implementation of sets for SRFI xxx

;;; This implementation is written in portable R5RS plus SRFIs 9, 23, and
;;; 69.  An implementation of all these SRFIs on top of R6RS is available.

;; Sets are records with two fields, an equality predicate and a
;; hash table.  The hash table maps each set member to #t.  We keep the
;; equality predicate separate and don't assume we can retrieve it from
;; the hash table.  This makes it easy to layer this SRFI over less
;; powerful hash table systems.
(define-record-type &set
  (raw-make-set set-equality set-dict)
  set?
  (set-equality set-equality)
  (set-dict set-dict))

;;; These procedures directly depend on the underlying representation.

;; Create an empty set with a specified equality predicate.
(define (make-set set-equality)
  (raw-make-set set-equality (make-hash-table set-equality)))

;; Return the number of elements in a set.
(define (set-size s)
  (hash-table-size (set-dict (set-check s))))

;; Return #t if an element e is a member of a set s.
(define (set-contains? s e)
  (hash-table-ref/default
    (set-dict (set-check s))
    e
    #f))

;; Add a new element.
(define (set-add! s e)
  (hash-table-set! (set-dict (set-check s)) e #t))

;; Remove an element.  If the element was present, return #t, otherwise #f.
(define (set-delete! s e)
  (hash-table-delete! (set-dict (set-check s)) e))

;; Map a function over a set, discarding the results.
(define (set-for-each proc s)
  (hash-table-walk (set-dict (set-check s)) (lambda (k v) (proc k))))

;; Set fold.  Returns nil if the set is empty.
(define (set-fold proc nil s)
  (hash-table-fold
    (set-dict (set-check s))
    (lambda (k v acc) (proc k acc))
    nil))

;; Returns the member of a set which is the same (in the sense of the
;; equality predicate) as the argument.
(define (set-value s e)
  (let loop ((keys (hash-table-keys (set-dict (set-check s)))))
    (cond
      ((null? keys)
       e)
      (((set-equality s) (car keys) e)
       (car keys))
      (else
       (loop (cdr keys))))))

;;; These procedures do not directly depend on the underlying representation.

;; Internal: signal an error if obj is not a set.
(define (set-check obj)
  (if (set? obj) obj (error "Set expected" obj)))

;; Internal: signal an error if s1 and s2 have different equality predicates.
(define (set-check-equalities s1 s2)
  (if (eq? (set-equality s1) (set-equality s2))
    #t
    (error "Set set-equality predicate discrepancy" s1 s2)))


;; Create a set with a specified equality predicate and populate it.
(define (set set-equality . elements)
  (let ((t (make-set set-equality)))
    (for-each (lambda (e) (set-add! t e)) elements)
    t))

;; Return a copy of a set, using the same equality predicate.
(define (set-copy s)
  (let ((t (set-empty-copy (set-check s))))
    (set-for-each (lambda (e) (set-add! t e)) s)
    t))

(define (set-empty-copy s)
  (make-set (set-equality (set-check s))))

;; Map a function over a set.
(define (set-map set-equality proc s)
  (let ((t (make-set set-equality)))
    (set-for-each (lambda (e) (set-add! t (proc e))) (set-check s))
    t))

;; Return a list of the set members.
(define (set->list s)
  (set-fold cons '() (set-check s)))

;; Create a set from an equality predicate and a list.
(define (list->set set-equality list)
  (let ((t (make-set set-equality)))
    (for-each (lambda (e) (set-add! t e)) list)
    t))

;; Return #t if all sets are equal, #f otherwise.
(define (set=? s . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-set=? (set-check s) (car sets))
     (apply set=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of set=?.
(define (dyadic-set=? s1 s2)
  (set-check-equalities s1 s2)
  (and (dyadic-set<=? s1 s2) (dyadic-set>=? s1 s2)))

;; Return #t if each set is a proper subset of the following set.
(define (set<? s . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-set<? (set-check s) (car sets))
     (apply set<? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of set<?.
(define (dyadic-set<? s1 s2)
  (set-check-equalities s1 s2)
  (not (dyadic-set>=? s1 s2)))

;; Return #t if each set is a proper superset of the following set.
(define (set>? s . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-set>? (set-check s) (car sets))
     (apply set>? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of set>?.
(define (dyadic-set>? s1 s2)
  (set-check-equalities s1 s2)
  (not (dyadic-set<=? s1 s2)))

;; Return #t if each set is a subset (proper or improper) of the following set.
(define (set<=? s . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-set<=? (set-check s) (car sets))
     (apply set<=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of set<=?.
(define (dyadic-set<=? s1 s2)
  (set-check-equalities s1 s2)
  (call/cc
    (lambda (return)
      (set-for-each
        (lambda (e)
          (if (set-contains? s2 e) #t (return #f)))
        s1)
      #t)))

(define (set>=? s . sets)
;; Return #t if each set is a superset (proper or improper) of the following set.
  (cond
    ((null? sets)
     #t)
    ((dyadic-set>=? (set-check s) (car sets))
     (apply set=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of set>=?.
(define (dyadic-set>=? s1 s2)
  (set-check-equalities s1 s2)
  (dyadic-set<=? s2 s1))

;; Return the union of all sets.
(define (set-union s . sets)
  (apply set-union! (set-copy (set-check s)) sets))

;; Return the union of all sets, destroying the first.
(define (set-union! s . sets)
  (set-check s)
  (for-each
    (lambda (set)
      (set-check-equalities s set)
      (set-for-each (lambda (e) (set-add! s e)) (set-check set)))
    sets)
  s)

;; Return the intersection of all sets.
(define (set-intersection s . sets)
  (apply set-intersection! (set-copy (set-check s)) sets))

;; Return the intersection of all sets, destroying the first.
(define (set-intersection! s . sets)
  (set-check s)
  (for-each
    (lambda (set) (dyadic-set-intersection! s (set-check set)))
    sets)
  s)

;; Internal: return the intersection of two sets, destroying the first.
(define (dyadic-set-intersection! s1 s2)
  (set-check-equalities s1 s2)
  (set-for-each
    (lambda (e)
      (if (set-contains? s2 e) #t (set-delete! s1 e)))
    s1)
  s1)

;; Return the asymmetric difference of s and the union of the other sets.
(define (set-difference s . sets)
  (apply set-difference! (set-copy (set-check s)) sets))

;; Asymmetric difference, destroying s.
(define (set-difference! s . sets)
  (set-check s)
  (for-each
    (lambda (set)
      (set-check-equalities s set)
      (set-for-each (lambda (e) (set-delete! s e)) (set-check set)))
    sets)
  s)

;; Exclusive or (symmetric difference) of two sets.
(define (set-xor s1 s2)
  (set-check-equalities s1 s2)
  (let ((t (set-copy (set-check s1))))
    (set-xor! t (set-check s2))))

;; Exclusive or, destroying the first set.
;; Perhaps not the most efficient implementation....
(define (set-xor! s1 s2)
  (set-check-equalities s1 s2)
  (let ((int (set-intersection s1 s2)))
    (set-difference! (set-union! s1 s2) int)))

;; Construct a set by unfolding
(define (set-unfold equivalence continue? mapper successor seed)
  (let loop ((s (make-set equivalence))
             (seed seed))
    (if (continue? seed)
      (let ((r (mapper seed)))
        (set-add! s r)
        (loop s (successor seed)))
      s)))

;; Filter and partition set
(define (set-filter pred s)
  (let ((t (set-empty-copy s)))
    (set-for-each
      (lambda (x) (if (pred x) (set-add! t x)))
      s)
    t))

(define (set-remove pred s)
  (let ((t (set-empty-copy s)))
    (set-for-each
      (lambda (x) (if (not (pred x)) (set-add! t x)))
      s)
    t))

(define (set-partition pred s)
  (let ((yes (set-empty-copy s))
        (no (set-empty-copy s)))
    (set-for-each
      (lambda (x)
        (if (pred x) (set-add! yes x) (set-add! no x)))
      s)
    (values yes no)))

;; Count matching elements of set
(define (set-count pred s)
  (let ((n 0))
    (set-for-each
      (lambda (x)
        (if (pred x) (set! n (+ n 1))))
      s)
    n))

;; Return #t if any element of s satisfies pred
(define (set-any? pred s)
  (call/cc
    (lambda (return)
      (set-for-each
        (lambda (e) (if (pred e) (return #t)))
        s)
      #f)))

;; Return #t if every element of s satisfies pred
(define (set-every? pred s)
  (call/cc
    (lambda (return)
      (set-for-each
        (lambda (e) (if (not (pred e)) (return #f)))
        s)
      #t)))

;; Return an arbitrary element of s that satisfies pred,
;; or invoke failure if none
(define (set-find pred s failure)
  (call/cc
    (lambda (return)
      (set-for-each
        (lambda (e) (if (pred e) (return e)))
        s)
      (failure))))

;; Internal: print the contents of a set, for debugging.
(define (print-set s out)
  (set-check s)
  (display "#<set " out)
  (set-for-each (lambda (e) (display e out) (display #\space out)) s)
  (display ">" out))
