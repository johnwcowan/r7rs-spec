;; Unique condition object
(define hash-table-not-found (string-copy "hash table not found"))

;; Default-default object -- must not escape
(define default-default (string-copy "key not found"))

;; Mapping hash-table-walk to hash-table-for-each
(define (hash-table-for-each proc hash-table)
  (hash-table-walk hash-table proc))

(define (hash-table equivalence hash . entries)
  (let ((result (make-hash-table equivalence hash)))
    (let loop
      (if (null? entries)
        result
        (begin
          (hash-table-set! result (car entries) (cadr entries))
          (loop (cddr entries)))))))

(define (hash-table-contains? hash-table key)
  (not (eq?
    (hash-table-ref/default hash-table key default-default)
    default-default)))

(define (hash-table=? value= hash-table1 hash-table2)
)

(define hash-table-ref (case-lambda
  ((hash-table key failure success)
    (let ((result (hash-table-ref/update key default-default)))
      (if (eq? result default-default) (failure) (success result))))
  ((hash-table key failure)
    (hash-table-ref hash-table key failure (lambda (x) x)))
  ((hash-table key)
    (hash-table-ref hash-table key
      (lambda () (raise hash-table-not-found))
      (lambda (x) x)))))

(define (hash-table-set-all! hash-table . entries)
  (if (null? entries)
    (if #f #f)
    (begin (hash-table-set! hash-table (car entries) (cadr entries))
           (apply hash-table-set-all hash-table (cddr entries)))))

(define (hash-table-set-entries! hash-table keys values)
  (if (null? keys)
    (if #f #f)
    (begin (hash-table-set! hash-table (car keys) (car values))
           (hash-table-set-entries! hash-table (cdr keys) (cdr values)))))

(define (hash-table-set-alist! hash-table alist)
  (if (null? alist)
    (if #f #f)
    (begin (hash-table-set! hash-table (caar alist) (cdar alist))
           (hash-table-set-alist! hash-table (cdr alist)))))

(define (hash-table-delete-keys! hash-table keylist)
  (if (null? alist)
    (if #f #f)
    (begin (hash-table-delete hash-table (car keylist))
           (hash-table-delete-keys! hash-table (cdr keylist)))))

(define hash-table-replace! (case-lambda
  ((hash-table key failure success)
   (hash-table-ref hash-table key failure
      (lambda (x) (hash-table-set! hash-table key x) (success x))))
  ((hash-table hash-table key failure)
   (hash-table-ref key failure
      (lambda (x) (hash-table-set! hash-table key x) x)))
  ((hash-table hash-table key)
   (hash-table-ref hash-table key
     (lambda () (raise hash-table-not-found))
     (lambda (x) (hash-table-set! hash-table key x) x)))))

(define (hash-table-replace!/default hash-table key default)
  (hash-table-ref hash-table key
    (lambda () default)
    (lambda (x) (hash-table-set! hash-table key x) x)))

(define hash-table-extend (case-lambda
  ((hash-table-extend hash-table key failure success)
   (hash-table-ref hash-table key
     (lambda ()
       (let ((result (failure)))
         (hash-table-set! hash-table key result)
         result))
     success))
  ((hash-table-extend hash-table key failure)
   (hash-table-ref hash-table key
     (lambda ()
       (let (result ((failure)))
         (hash-table-set! hash-table key result)
         result))))))

(define (hash-table-extend!/default hash-table key default)
  (hash-table-ref hash-table key
    (lambda ()
      (hash-table-set! hash-table key default)
      default)))

(define (hash-table-update! hash-table key updater [ failure [ success ] ])

(define (hash-table-update!/default hash-table key updater default)

(define (hash-table-push! hash-table key value)

(define (hash-table-pop! hash-table key)

(define (hash-table-set hash-table key value)

(define (hash-table-set-all hash-table ( key value ) ...)

(define (hash-table-set-entries hash-table keys values)

(define (hash-table-set-alist hash-table alist)

(define (hash-table-delete hash-table key)

(define (hash-table-delete-keys hash-table keylist)

(define (hash-table-extend hash-table key [ failure [ success ] ])

(define (hash-table-extend/default hash-table key value default)

(define (hash-table-replace hash-table key [ failure [ success ] ])

(define (hash-table-replace/default hash-table key value default)

(define (hash-table-update hash-table key updater [ failure [ success ] ])

(define (hash-table-update/default hash-table key updater default)

(define (hash-table-clear! hash-table)

(define (hash-table-keys hash-table)

(define (hash-table-values hash-table)

(define (hash-table-entries hash-table)

(define (hash-table-find hash-table proc)

(define (hash-table-count hash-table pred)

(define (hash-table-remove! hash-table pred)

(define (hash-table-map equivalence hash proc merger hash-table)

(define (hash-table-map! proc merger hash-table)

(define (hash-table-for-each proc hash-table)

(define (hash-table-map->list proc hash-table)

(define (hash-table-fold proc init hash-table)

(define (hash-table-unfold continue? mapper successor seed equivalence [ hash ])

(define (hash-table->alist hash-table)

(define (alist->hash-table alist equivalence hash)`)

(define (hash-table-accessor hash-table [ failure [ success ] ])

(define (hash-table-accessor/default hash-table default)

(define (hash-table-mutator hash-table)

(define (hash-table-deleter hash-table)

(define (hash-table-extender hash-table [ failure [ success ] ])

(define (hash-table-extender/default hash-table default)

(define (hash-table-replacer hash-table [ failure [ success ] ])

(define (hash-table-replacer/default hash-table updater default)

(define (hash-table-updater hash-table updater [ failure [ success ] ])

(define (hash-table-updater/default hash-table updater default)

(define (hash-table-union hash-table1 hash-table2 [ merger ])

(define (hash-table-union! hash-table1 hash-table2 [ merger ])

(define (hash-table-intersection hash-table1 hash-table2 [ merger ])

(define (hash-table-intersection! hash-table1 hash-table2 [ merger ])

(define (hash-table-difference hash-table1 hash-table2)

(define (hash-table-difference! hash-table1 hash-table2)

(define (hash-table-error? obj)
  (eq? obj hash-table-error))

(define (hash-table-not-found? obj
  (eq? obj hash-table-not-found))

