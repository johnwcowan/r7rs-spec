;;;; Implementation of enum sets for SRFI xxx

;;; This implementation is written in portable R5RS plus SRFIs 9 and 23
;;; and bytevectors (we use the R7RS version here).  An implementation
;;; of all these SRFIs on top of R6RS is available.

;; Enum sets are records with two fields, a et (a wrapper around
;; a vector of symbols) and an integer-set.  Let n be the position of a
;; symbol in the vector.  If the integer-set includes n, then the enum
;; set includes that symbol.

(define-record-type &enum-set
  (raw-make-enum-set enum-type enum-is)
  enum-set?
  (enum-type enum-type)
  (enum-is enum-is))

;; In fact a type has not only a record of symbols but a record of values
;; as well.  This allows enum-types to accommodate C and C++ enumerations,
;; where the programmer gets to pick some or all of the values.  

(define-record-type &enum-type
  (raw-make-enum-type sv vv)
  enum-type?
  (sv sv)
  (vv vv))

;;; These procedures directly depend on the underlying representation.

;; Internal: Return the index of a symbol in an enum-set, or #f if none
;; This does brute-force linear search, on the assumption that enum-types
;; tend to be small.  It could be (and almost was) replaced by a hash table.
(define (enum-type-index et sym)
  (let ((sv (sv (enum-type-check et))))
    (count-up (return #f) (i 0 (vector-length sv))
      (if (eqv? sym (vector-ref sv i)) (return i)))))

;; Internal: Return the index of a symbol in the type of an enum-set,
;; or #f if none
(define (enum-set-index es sym)
  (enum-type-index (enum-type es) sym))

;; Return the value of a symbol in an enum-type, or #f if none
(define (enum-type-symbol-value et sym)
  (let ((i (enum-type-index et sym)))
    (if i (vector-ref (vv et) i) #f)))

;; Return the first symbol with a given value, or #f if none
(define (enum-type-symbol et val)
  (let ((vv (vv (enum-type-check et))))
    (count-up (return #f) (i 0 (vector-length vv))
      (if (eqv? (vector-ref vv i) val)
        (return (vector-ref (sv et) i))))))

;; Create an enum-type from a list of symbols and (symbol . value) pairs
(define (make-enum-type symbols)
  (let* ((length (length symbols))
         (sv (make-vector length))
         (vv (make-vector length)))
    (let loop ((symbols symbols)
               (i 0)
               (last -1))
      (cond
        ((null? symbols)
         (raw-make-enum-type sv vv))
        ((symbol? (car symbols))
         (vector-set! sv i (car symbols))
         (vector-set! vv i (+ last 1))
         (loop (cdr symbols) (+ i 1) (+ last 1)))
        ((pair? (car symbols))
         (let ((sym (caar symbols)) (val (cdar symbols)))
           (if (not (symbol? sym))
             (error "make-enum-type: not a symbol" sym))
           (if (not (and (integer? val) (exact? val)))
             (error "make-enum-type: not an exact integer" val))
           (vector-set! sv i sym)
           (vector-set! vv i val)
           (loop (cdr symbols) (+ i 1) val)))
         (else
           (error "make-enum-type: not a symbol" (car symbols)))))))

;; Return an alist of the symbols in an enum-type and their values
(define (enum-type->alist et)
  (enum-type-check et)
  (let ((sv (sv et)) (vv (vv et))(r '()))
      (count-up (i 0 (vector-length sv))
        (set! r (cons (cons (vector-ref sv i) (vector-ref vv i)) r)))
      (reverse r)))

;; Internal: limit of an enum-set
(define (enum-limit enum-set)
  (enum-type-limit (enum-type (enum-check enum-set))))

;; Internal: limit of an enum-type
(define (enum-type-limit et)
  (vector-length (sv (enum-type-check et))))

;; Create an empty enum-set with a specified enum-type.
(define (make-enum-set et)
  (raw-make-enum-set
    et
    (make-integer-set (enum-type-limit et))))

;; Enum-set-specific: create a full enum-set with a specified enum-type.
(define (make-universal-enum-set et)
  (raw-make-enum-set
    et
    (make-universal-integer-set (enum-type-limit et))))

;; Return the number of elements in an enum-set.
(define (enum-set-size es)
  (enum-check es)
  (let ((count 0) (enum-is (enum-is es)))
    (count-up (i 0 (enum-limit es))
      (if (integer-set-member? enum-is i)
        (set! count (+ count 1))))
    count))

;; Return #t if an element e is a member of an enum-set es.
(define (enum-set-member? es e)
  (let ((i (enum-set-index (enum-check es) e)))
    (if i (integer-set-member? (enum-is es) i) #f)))

;; Add a new element.
(define (enum-set-add! es e)
  (integer-set-add!
    (enum-is (enum-check-member es e))
    (enum-set-index es e)))

;; Remove an element.  If the element was present, return #t, otherwise #f.
(define (enum-set-delete! es e)
  (integer-set-delete!
    (enum-is (enum-check-member es e))
    (enum-set-index es e)))

;; Map a function over an enum-set, discarding the results.
(define (enum-set-for-each proc es)
  (let ((enum-is (enum-is (enum-check es)))
        (sv (sv (enum-type es))))
    (integer-set-for-each
      (lambda (i) (proc (vector-ref sv i)))
      enum-is)))

;; Enum-set fold.  Returns nil if the enum-set is empty.
(define (enum-set-fold proc nil es)
  (let ((enum-is (enum-is (enum-check es)))
        (sv (sv (enum-type es))))
    (integer-set-fold
      (lambda (i acc) (proc (vector-ref sv i) acc))
      nil
      enum-is)))

;; Enum-set-specific: fast copy
(define (enum-set-copy es)
  (enum-check es)
  (raw-make-enum-set (enum-type es) (integer-set-copy (enum-is es))))

(define (enum-set-empty-copy es)
  (enum-check es)
  (raw-make-enum-set (enum-type es) (make-integer-set (enum-limit es))))

;; Enum-set-specific: complement an enum-set
(define (enum-set-complement! es)
  (let* ((is (enum-is (enum-check es)))
         (limit (limit is))
         (bv (bv is)))
    (count-up (return es) (i 0 limit)
      (bytevector-u8-set! bv i (- 1 (bytevector-u8-ref bv i))))))

;; Enum-set-specific: correspond to integer-set versions
(define (enum-set-min es)
  (enum-check es)
  (let ((et (enum-type es))
        (result (integer-set-min (enum-is es))))
    (if result (vector-ref (sv et) result) #f)))

(define (enum-set-max es)
  (enum-check es)
  (let ((et (enum-type es))
        (result (integer-set-max (enum-is es))))
    (if result (vector-ref (sv et) result) #f)))

(define (enum-set-delete-min! es)
  (enum-check es)
  (let ((et (enum-type es))
        (result (integer-set-delete-min! (enum-is es))))
    (if result (vector-ref (sv et) result) #f)))

(define (enum-set-delete-max! es)
  (enum-check es)
  (let ((et (enum-type es))
        (result (integer-set-delete-max! (enum-is es))))
    (if result (vector-ref (sv et) result) #f)))

;;; These procedures do not directly depend on the underlying representation.

;; Internal: signal an error if obj is not an enum-type.
(define (enum-type-check obj)
  (if (enum-type? obj) obj (error "Enum-type expected" obj)))

;; Internal: signal an error if obj is not an enum-set.
(define (enum-check obj)
  (if (enum-set? obj) obj (error "Enum-set expected" obj)))

;; Internal: signal an error if n is out of range for es.
(define (enum-check-member es sym)
  (if (enum-set-index es sym)
    es
    (error "Enum-set value out of range" es sym)))

;; Internal: check that the ets of is1 and is2 are the same.
(define (enum-check-types is1 is2)
  (if (eq? (enum-type is1) (enum-type is2))
    #t
    (error "Enum-sets have different enum-types" is1 is2)))

;; Create an enum-set with a specified enum-type and populate it.
(define (enum-set et . elements)
  (let ((t (make-enum-set et)))
    (for-each (lambda (e) (enum-set-add! t e)) elements)
    t))

;; Return a copy of an enum-set, using the same enum-type.
;; Map a function over an enum-set.
(define (enum-set-map et proc es)
  (let ((t (make-enum-set et)))
    (enum-set-for-each (lambda (e) (enum-set-add! t (proc e))) (enum-check es))
    t))

;; Return a list of the enum-set members.
(define (enum-set->list es)
  (enum-set-fold cons '() (enum-check es)))

;; Create an enum-set from an enum-type and a list.
(define (list->enum-set et list)
  (let ((t (make-enum-set et)))
    (for-each (lambda (e) (enum-set-add! t e)) list)
    t))

;; Return #t if all sets are equal, #f otherwise.
(define (enum-set=? es . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-enum-set=? (enum-check es) (car sets))
     (apply enum-set=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of enum-set=?.
(define (dyadic-enum-set=? is1 is2)
  (enum-check-types is1 is2)
  (and (dyadic-enum-set<=? is1 is2) (dyadic-enum-set>=? is1 is2)))

;; Return #t if each enum-set is a proper subset of the following enum-set.
(define (enum-set<? es . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-enum-set<? (enum-check es) (car sets))
     (apply enum-set<? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of enum-set<?.
(define (dyadic-enum-set<? is1 is2)
  (enum-check-types is1 is2)
  (not (dyadic-enum-set>=? is1 is2)))

;; Return #t if each enum-set is a proper superset of the following enum-set.
(define (enum-set>? es . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-enum-set>? (enum-check es) (car sets))
     (apply enum-set>? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of enum-set>?.
(define (dyadic-enum-set>? is1 is2)
  (enum-check-types is1 is2)
  (not (dyadic-enum-set<=? is1 is2)))

;; Return #t if each enum-set is a subset (proper or improper) of the following enum-set.
(define (enum-set<=? es . sets)
  (cond
    ((null? sets)
     #t)
    ((dyadic-enum-set<=? (enum-check es) (car sets))
     (apply enum-set<=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of enum-set<=?.
(define (dyadic-enum-set<=? is1 is2)
  (enum-check-types is1 is2)
  (call/cc
    (lambda (return)
      (enum-set-for-each
        (lambda (e)
          (if (enum-set-member? is2 e) #t (return #f)))
        is1)
      #t)))

(define (enum-set>=? es . sets)
;; Return #t if each enum-set is a superset (proper or improper) of the following enum-set.
  (cond
    ((null? sets)
     #t)
    ((dyadic-enum-set>=? (enum-check es) (car sets))
     (apply enum-set=? (car sets) (cdr sets)))
    (else
     #f)))

;; Internal: dyadic version of enum-set>=?.
(define (dyadic-enum-set>=? is1 is2)
  (enum-check-types is1 is2)
  (dyadic-enum-set<=? is2 is1))

;; Return the union of all sets.
(define (enum-set-union es . sets)
  (apply enum-set-union! (enum-set-copy (enum-check es)) sets))

;; Return the union of all sets, destroying the first.
(define (enum-set-union! es . sets)
  (enum-check es)
  (for-each
    (lambda (enum-set)
      (enum-check-types es enum-set)
      (enum-set-for-each (lambda (e) (enum-set-add! es e)) (enum-check enum-set)))
    sets)
  es)

;; Return the intersection of all sets.
(define (enum-set-intersection es . sets)
  (apply enum-set-intersection! (enum-set-copy (enum-check es)) sets))

;; Return the intersection of all sets, destroying the first.
(define (enum-set-intersection! es . sets)
  (enum-check es)
  (for-each
    (lambda (enum-set) (dyadic-enum-set-intersection! es (enum-check enum-set)))
    sets)
  es)

;; Internal: return the intersection of two sets, destroying the first.
(define (dyadic-enum-set-intersection! is1 is2)
  (enum-check-types is1 is2)
  (enum-set-for-each
    (lambda (e)
      (if (enum-set-member? is2 e) #t (enum-set-delete! is1 e)))
    is1)
  is1)

;; Return the asymmetric difference of es and the union of the other sets.
(define (enum-set-difference es . sets)
  (apply enum-set-difference! (enum-set-copy (enum-check es)) sets))

;; Asymmetric difference, destroying es.
(define (enum-set-difference! es . sets)
  (enum-check es)
  (for-each
    (lambda (enum-set)
      (enum-check-types es enum-set)
      (enum-set-for-each (lambda (e) (enum-set-delete! es e)) (enum-check enum-set)))
    sets)
  es)

;; Exclusive or (symmetric difference) of two sets.
(define (enum-set-xor es1 es2)
  (enum-check-types es1 es2)
  (let ((t (enum-set-copy (enum-check es1))))
    (enum-set-xor! t (enum-check es2))))

;; Exclusive or, destroying the first enum-set.
;; Not the most efficient implementation....
(define (enum-set-xor! es1 es2)
  (enum-check-types es1 es2)
  (let ((int (enum-set-intersection es1 es2)))
    (enum-set-difference! (enum-set-union! es1 es2) int)))

;; Enum-set-specific: project es into a new enum-set with type et
(define (enum-set-projection es et)
  (let ((r (make-enum-set et)))
    (enum-set-for-each
      (lambda (e)
        (if (enum-type-index et e)
          (enum-set-add! r e)))
      es)
    r))

;; Enum-set-specific: return the complement of an enum-set
(define (enum-set-complement es)
  (enum-set-complement! (enum-set-copy (enum-check es))))

(define (enum-value=? et . syms)
  (apply = (map (lambda (sym) (enum-type-symbol-value et sym)) syms)))

(define (enum-value<? et . syms)
  (apply < (map (lambda (sym) (enum-type-symbol-value et sym)) syms)))

(define (enum-value>? et . syms)
  (apply > (map (lambda (sym) (enum-type-symbol-value et sym)) syms)))

(define (enum-value<=? et . syms)
  (apply <= (map (lambda (sym) (enum-type-symbol-value et sym)) syms)))

(define (enum-value>=? et . syms)
  (apply >= (map (lambda (sym) (enum-type-symbol-value et sym)) syms)))

;; Construct a set by unfolding
(define (enum-set-unfold et continue? mapper successor seed)
  (let loop ((s (make-enum-set et))
             (seed seed))
    (if (continue? seed)
      (let ((r (mapper seed)))
        (enum-set-add! s r)
        (loop s (successor seed)))
      s)))

;; Return enum-set that contains elements that satisfy pred
(define (enum-set-filter pred es)
  (let ((t (enum-set-empty-copy es)))
    (enum-set-for-each
      (lambda (x) (if (pred x) (enum-set-add! t x)))
      es)
    t))

;; Return enum-set that contains elements that don't satisfy pred
(define (enum-set-remove pred es)
  (let ((t (enum-set-empty-copy es)))
    (enum-set-for-each
      (lambda (x) (if (not (pred x)) (enum-set-add! t x)))
      es)
    t))

;; Partition enum-set into elements that do and don't satisfy pred
(define (enum-set-partition pred es)
  (let ((yes (enum-set-empty-copy es))
        (no (enum-set-empty-copy es)))
    (enum-set-for-each
      (lambda (x)
        (if (pred x) (enum-set-add! yes x) (enum-set-add! no x)))
      es)
    (values yes no)))

;; Count matching elements of enum-set
(define (enum-set-count pred es)
  (let ((n 0))
    (enum-set-for-each
      (lambda (x)
        (if (pred x) (set! n (+ n 1))))
      es)
    n))

;; Return #t if any element of es satisfies pred
(define (enum-set-any? pred es)
  (call/cc
    (lambda (return)
      (enum-set-for-each
        (lambda (e) (if (pred e) (return #t)))
        es)
      #f)))

;; Return #t if every element of es satisfies pred
(define (enum-set-every? pred es)
  (call/cc
    (lambda (return)
      (enum-set-for-each
        (lambda (e) (if (not (pred e)) (return #f)))
        es)
      #t)))

;; Return an arbitrary element of es that satisfies pred,
;; or invoke failure if none
(define (enum-set-find pred es failure)
  (call/cc
    (lambda (return)
      (enum-set-for-each
        (lambda (e) (if (pred e) (return e)))
        es)
      (failure))))

;; Internal: print the contents of an enum-set, for debugging.
(define (print-enum-set es out)
  (enum-check es)
  (display "#<enum-set " out)
  (enum-set-for-each (lambda (e) (display e out) (display #\space out)) es)
  (display ">" out))

;; Internal: print the contents of an enum-type, for debugging.
(define (print-enum-type et out)
  (enum-type-check et)
  (display "#<enum-type " out)
  (display (sv et))
  (display ">" out))
