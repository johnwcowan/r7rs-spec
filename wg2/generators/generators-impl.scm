;; Chibi Scheme versions of any and every

(define (any pred ls)
  (if (null? (cdr ls))
      (pred (car ls))
      ((lambda (x) (if x x (any pred (cdr ls)))) (pred (car ls)))))

(define (every pred ls)
  (if (null? (cdr ls))
      (pred (car ls))
      (if (pred (car ls)) (every pred (cdr ls)) #f)))



;; make-generator
(define (make-generator . args)
        (lambda () (if (null? args)
                       (eof-object)
                       (let ((next (car args)))
                            (set! args (cdr args))
                            next))))


;; make-circular-generator
(define (make-circular-generator . args)
        (let ((loopgen (apply make-generator args)))
             (lambda () (let ((next (loopgen)))
                             (if (eof-object? next)
                                 (begin (set! loopgen (apply make-generator args))
                                        (loopgen))
                                 next)))))


;; make-iota-generator
(define make-iota-generator
        (case-lambda (() (make-iota-generator +inf.0 0 1))
                     ((count) (make-iota-generator count 0 1))
                     ((count start) (make-iota-generator count start 1))
                     ((count start step)
                      (set! start (- (+ start step) step))
                      (lambda () (if (> count 0)
                                     (let ((v start))
                                          (set! start (+ start step))
                                          (set! count (- count 1))
                                          v)
                                     (eof-object))))))

;; make-range-generator
(define make-range-generator
        (case-lambda ((start end) (make-range-generator start end 1))
                     ((start end step)
                      (set! start (- (+ start step) step))
                      (lambda () (if (< start end)
                                     (let ((v start))
                                          (set! start (+ start step))
                                          v)
                                     (eof-object))))))


;; make-tabulation-generator
(define make-tabulation-generator
        (case-lambda ((proc) (make-tabulation-generator +inf.0 proc))
                     ((k proc)
                      (define n 0)
                      (lambda () (if (< n k)
                                     (let ((v n))
                                          (set! n (+ n 1))
                                          (proc v))
                                     (eof-object))))))


;; make-coroutine-generator
(define (make-coroutine-generator proc)
        (define return #f)
        (define resume #f)
        (define yield (lambda (v) (call/cc (lambda (r) (set! resume r) (return v)))))
        (lambda () (call/cc (lambda (cc) (set! return cc)
                                         (if resume
                                             (resume (if #f #f))  ; void? or yield again?
                                             (begin (proc yield)
                                                    (set! resume (lambda (v) (return (eof-object))))
                                                    (return (eof-object))))))))


;; make-list-generator
(define (make-list-generator lst)
        (lambda () (if (null? lst)
                       (eof-object)
                       (let ((next (car lst)))
                            (set! lst (cdr lst))
                            next))))


;; make-vector-generator
(define make-vector-generator
        (case-lambda ((vec) (make-vector-generator vec 0 (vector-length vec))) 
                     ((vec start) (make-vector-generator vec start (vector-length vec))) 
                     ((vec start end)
                      (lambda () (if (>= start end)
                                     (eof-object)
                                     (let ((next (vector-ref vec start)))
                                          (set! start (+ start 1))
                                          next))))))


;; make-reverse-vector-generator
(define make-reverse-vector-generator
        (case-lambda ((vec) (make-reverse-vector-generator vec 0 (vector-length vec))) 
                     ((vec start) (make-reverse-vector-generator vec start (vector-length vec))) 
                     ((vec start end)
                      (lambda () (if (>= start end)
                                     (eof-object)
                                     (let ((next (vector-ref vec (- end 1))))
                                          (set! end (- end 1))
                                          next))))))


;; make-string-generator
(define make-string-generator
        (case-lambda ((str) (make-string-generator str 0 (string-length str))) 
                     ((str start) (make-string-generator str start (string-length str))) 
                     ((str start end)
                      (lambda () (if (>= start end)
                                     (eof-object)
                                     (let ((next (string-ref str start)))
                                          (set! start (+ start 1))
                                          next))))))


(define make-bytevector-generator
        (case-lambda ((str) (make-bytevector-generator str 0 (bytevector-length str))) 
                     ((str start) (make-bytevector-generator str start (bytevector-length str))) 
                     ((str start end)
                      (lambda () (if (>= start end)
                                     (eof-object)
                                     (let ((next (bytevector-u8-ref str start)))
                                          (set! start (+ start 1))
                                          next))))))


;; make-bits-generator
(define (make-bits-generator n)
        (if (negative? n)
          (let ((m (- (+ n 1))))
            (lambda ()
              (if (= m 0)
                (eof-object)
                (let ((lowbit (if (odd? m) 1 0)))
                  (set! m (/ (- m lowbit) 2))
                  (= lowbit 0)))))
          (lambda ()
            (if (= n 0)
              (eof-object)
              (let ((lowbit (if (odd? n) 1 0)))
                (set! n (/ (- n lowbit) 2))
                (= lowbit 1))))))
        


;; make-port-generator
(define (make-port-generator port reader)
        (lambda () (reader port)))
        


;; make-port-sexp-generator
(define (make-port-sexp-generator port)
        (lambda () (read port)))
        


;; make-port-line-generator
(define (make-port-line-generator port)
        (lambda () (read-line port)))
        


;; make-port-char-generator
(define (make-port-char-generator port)
        (lambda () (read-char port)))
        


;; make-port-byte-generator
(define (make-port-byte-generator port)
	(lambda () (read-u8 port)))
        


;; make-for-each-generator
(define (make-for-each-generator for-each obj)
        (make-coroutine-generator (lambda (yield) (for-each yield obj))))


;; make-unfold-generator
(define (make-unfold-generator stop? mapper successor seed)
        (make-coroutine-generator (lambda (yield) 
            (let loop ((s seed))
                 (if (stop? s)
                     (if #f #f)
                     (begin (yield (mapper s))
                            (loop (successor s))))))))


;; gcons*
(define (gcons* . args)
        (lambda () (if (null? args)
                       (eof-object)
                       (if (= (length args) 1)
                           ((car args))
                           (let ((v (car args)))
                                (set! args (cdr args))
                                v)))))
        


;; gappend
(define (gappend . args)
        (lambda () (if (null? args)
                       (eof-object)
                       (let loop ((v ((car args))))
                                 (if (eof-object? v)
                                     (begin (set! args (cdr args))
                                            (if (null? args)
                                                (eof-object) 
                                                (loop ((car args)))))
                                     v)))))
        


;; gconcatenate
(define (gconcatenate . args)
        (define g (make-generator))
        (lambda () (let loop ((v (g)))
                             (if (eof-object? v)
                                 (if (null? args)
                                     v
                                    (let ((nextg ((car args))))
                                         (if (eof-object? nextg)
                                             (begin (set! args (cdr args))
                                                    (loop v))
                                             (begin (set! g nextg)
                                                    (loop (g))))))
                                 v))))
             


;; gmerge
(define (gmerge comp . args)
        (define argsvec (list->vector args))
        (define nextvec #f)
        (if (null? args)
            (make-generator)
            (lambda () (when (not nextvec) (set! nextvec (list->vector (map (lambda (g) (g)) args))))
                       (let-values (((low-i low-v) 
                                    (let loop ((i 0) (low-i -1) (low-v (eof-object)))
                                         (if (< i (vector-length nextvec))
                                             (let ((v (vector-ref nextvec i)))
                                                 (if (or (eof-object? low-v) 
                                                         (and (not (eof-object? v)) (<? comp v low-v)))
                                                     (loop (+ 1 i) i v)
                                                     (loop (+ 1 i) low-i low-v)))
                                             (values low-i low-v)))))
                                  (vector-set! nextvec low-i ((vector-ref argsvec low-i)))
                                  low-v))))
             


;; gunion
(define (gunion comp . args)
        (define g (apply gmerge (cons comp args)))
        (define next #f)
        (lambda () (when (not next) (set! next (g)))
                   (if (eof-object? next)
                       next
                       (let loop ((v (g)))
                            (if (=? comp next v)
                                (loop (g))
                                (let ((cur next))
                                     (set! next v)
                                     cur))))))
             


;; gintersection
(define (gintersection comp . args)
        (define g (apply gmerge (cons comp args)))
        (define n (length args))
        (define next #f)
        (lambda () (when (not next) (set! next (g)))
                   (let top ()
                        (if (eof-object? next)
                            next
                            (let loop ((i 1) (v (g)))
                                 (if (=? comp next v)
                                      (loop (+ 1 i) (g))
                                      (let ((cur next))
                                           (set! next v)
                                           (if (= i n)
                                               cur
                                               (top)))))))))
             


;; gmap
(define (gmap proc . gens)
        (lambda () (let ((results (map (lambda (g) (g)) gens)))
                        (if (every (lambda (result) (not (eof-object? result))) results)
                            (apply proc results)
                            (eof-object)))))
             


;; gfold
(define (gfold proc seed . gens)
        (lambda () (let ((results (map (lambda (g) (g)) gens)))
                        (if (every (lambda (result) (not (eof-object? result))) results)
                            (let-values (((value newseed) (apply proc (append results (list seed)))))
                              (set! seed newseed)
                              value)
                            (eof-object)))))
             


;; gfilter
(define (gfilter pred gen)
        (lambda () (let loop ()
                        (let ((next (gen)))
                             (if (or (eof-object? next) 
                                     (pred next))
                                 next
                                 (loop))))))
             


;; gfilter-map
(define (gfilter pred gen)
        (lambda () (let loop ()
                        (let ((next (gen)))
                             (if (or (eof-object? next) 
                                     (pred next))
                                 next
                                 (loop))))))
             


;; gremove
(define (gremove pred gen)
        (gfilter (lambda (v) (not (pred v))) gen))
             


;; gfilter-map
(define (gfilter-map proc . gens)
        (define g (apply gmap (cons proc gens)))
        (lambda () (let loop ((next (g)))
                        (if (eof-object? next)
                            next
                            (if next
                                next
                                (loop (g)))))))
             


;; gstate-filter
(define (gstate-filter proc seed gen)
        (make-coroutine-generator (lambda (yield)
            (let loop ((v (gen)))
                 (if (eof-object? v)
                     v
                     (let-values (((b s) (proc v seed)))
                                 (set! seed s)
                                 (when b (yield v))
                                 (loop (gen))))))))
             


;; gbuffer-filter
(define (gbuffer-filter proc seed gen)
        (make-coroutine-generator (lambda (yield)
            (let loop ((v (gen)))
                 (if (eof-object? v)
                     v
                     (let-values (((buff s) (proc v seed)))
                                 (set! seed s)
                                 (if (null? buff)
                                     (loop (gen))
                                     (let loop2 ((b buff))
                                          (if (null? b)
                                              (loop (gen))
                                              (begin (yield (car b))
                                                     (loop2 (cdr b))))))))))))
             


;; gtake
(define gtake
        (case-lambda ((gen k) (gtake gen k (eof-object)))
                     ((gen k padding)
                      (make-coroutine-generator (lambda (yield)
                          (if (> k 0) 
                              (let loop ((i 0) (v (gen)))
                                   (begin (if (eof-object? v) (yield padding) (yield v))
                                          (if (< (+ 1 i) k)
                                              (loop (+ 1 i) (gen))
                                              (eof-object))))
                              (eof-object)))))))
             


;; gdrop
(define (gdrop gen k)
        (define (skip k)
                (if (>= 0 k)
                    (set! skip #f)
                    (begin (gen)
                           (skip (- k 1)))))
        (lambda () (when skip (skip k))
                   (gen)))
             


;; gdrop-while
(define (gdrop-while pred gen)
        (define (skip)
                (let ((next (gen)))
                     (if (pred next)
                         (skip)
                         (begin (set! gen (gcons* next gen))
                                (set! skip #f)))))
        (lambda () (when skip (skip))
                   (gen)))
             


;; gtake-while
(define (gtake-while pred gen)
        (lambda () (let ((next (gen)))
                        (if (eof-object? next)
                            next
                            (if (pred next)
                                next
                                (begin (set! gen (make-generator))
                                       (gen)))))))
             


;; gpairs
(define (gpairs car-gen cdr-gen)
        (lambda () (let ((car-next (car-gen)) 
                         (cdr-next (cdr-gen)))
                        (if (or (eof-object? car-next)
                                (eof-object? cdr-next)) 
                            (eof-object)
                            (cons car-next cdr-next)))))
             


;; gtuple
(define (gtuple . gens)
        (lambda () (let ((tuple (map (lambda (g) (g)) gens)))
                        (if (every (lambda (v) (not (eof-object? v))) tuple)
                            tuple
                            (eof-object)))))
             


;; glists
(define glists
        (case-lambda ((sizer item-gen)
                      (when (number? sizer)
                            (set! sizer (make-circular-generator sizer)))
                      (lambda () (let ((size (sizer)))
                                      (let loop ((i 0) (lst '()))
                                           (if (= i size)
                                               (reverse lst)
                                               (let ((next (item-gen)))
                                                    (if (eof-object? next)
                                                        (if (null? lst) next (reverse lst))
                                                        (loop (+ 1 i) (cons next lst)))))))))
                     ((sizer item-gen padding)
                      (define (pad lst csize tsize)
                              (if (= csize tsize)
                                  lst
                                  (pad (cons padding lst) (+ 1 csize) tsize)))
                      (when (number? sizer)
                            (set! sizer (make-circular-generator sizer)))
                      (lambda () (let ((size (sizer)))
                                      (let loop ((i 0) (lst '()))
                                           (if (= i size)
                                               (reverse lst)
                                               (let ((next (item-gen)))
                                                    (if (eof-object? next)
                                                        (if (null? lst) next (reverse (pad lst (length lst) size)))
                                                        (loop (+ 1 i) (cons next lst)))))))))))
             


;; gvectors
(define (gvectors . args)
        (gmap list->vector (apply glists args)))
             


;; gstrings
(define (gstrings . args)
        (gmap list->string (apply glists args)))
             


;; gdelete
(define gdelete
        (case-lambda ((item gen)
                      (lambda () (let loop ((v (gen)))
                                      (if (equal? item v)
                                          (loop (gen))
                                          v))))
                     ((item gen ==)
                      (lambda () (let loop ((v (gen)))
                                      ; change to (=? == item v) if using srfi 114 for this...
                                      (if (== item v)
                                          (loop (gen))
                                          v))))))
             


;; gdelete-neighbor-dups
(define gdelete-neighbor-dups
        (case-lambda ((gen)
                      (define firsttime #t)
                      (define prev #f)
                      (lambda () (if firsttime
                                     (begin (set! firsttime #f)
                                            (set! prev (gen))
                                            prev)
                                     (let loop ((v (gen)))
                                          (if (equal? prev v)
                                              (loop (gen))
                                              (begin (set! prev v)
                                                     v))))))
                     ((gen ==)
                      (define firsttime #t)
                      (define prev #f)
                      (lambda () (if firsttime
                                     (begin (set! firsttime #f)
                                            (set! prev (gen))
                                            prev)
                                     (let loop ((v (gen)))
                                          (if (== prev v)  ; or (=? == prev v)
                                              (loop (gen))
                                              (begin (set! prev v)
                                                     v))))))))


;; generator->list
(define generator->list
        (case-lambda ((gen) (generator->list gen +inf.0))
                     ((gen n) 
                      (let ((next (gen)))
                           (if (or (eof-object? next)
                                   (= 0 n))
                               '()
                               (cons next (generator->list gen (- n 1))))))))
     


;; generator->reverse-list
(define generator->reverse-list
        (case-lambda ((gen) (generator->reverse-list gen +inf.0))
                     ((gen n)
                      (define (build-reversed lst m)
                              (let ((next (gen)))
                                   (if (or (eof-object? next)
                                           (>= m n))
                                       lst
                                       (build-reversed (cons next lst) (+ 1 m)))))
                      (build-reversed '() 0))))
     


;; generator-fold
(define (generator-fold f seed . gs)
        (define (inner-fold seed)
                (let ((vs (map (lambda (g) (g)) gs)))
                     (if (any eof-object? vs)
                         seed
                         (inner-fold (apply f (append vs (list seed)))))))
        (inner-fold seed))
     


;; generator-for-each
(define (generator-for-each f . gs)
        (let loop ()
             (let ((vs (map (lambda (g) (g)) gs)))
                  (if (any eof-object? vs)
                      (if #f #f)
                      (begin (apply f vs) 
                             (loop))))))


;; generator-collect
(define (generator-collect f . gs)
        (let loop ((lst '()))
             (let ((vs (map (lambda (g) (g)) gs)))
                  (if (any eof-object? vs)
                      (reverse lst)
                      (loop (cons (apply f vs) lst))))))


;; generator-last
(define (generator-last g)
        (generator-fold (lambda (v acc) v) '() g))


;; generator-find
(define (generator-find pred g)
        (let loop ((v (g)))
                  ; A literal interpretation might say it only terminates on #eof if (pred #eof) but I think this makes more sense...
                  (if (or (pred v) (eof-object? v))
                      v
                      (loop (g)))))


;; generator-length
(define (generator-length g)
        (generator-fold (lambda (v n) (+ 1 n)) 0 g))


;; generator-count
(define (generator-count pred g)
        (generator-fold (lambda (v n) (if (pred v) (+ 1 n) n)) 0 g))


;; generator-any
(define (generator-any pred g)
        (let loop ((v (g)))
             (if (eof-object? v)
                 #f
                 (if (pred v)
                     #t
                     (loop (g))))))


;; generator-every
(define (generator-every pred g)
        (let loop ((v (g)))
             (if (eof-object? v)
                 #t
                 (if (pred v)
                     (loop (g))
                     #f ; the spec would have me return #f, but I think it must simply be wrong...
                     ))))


;; generator=?
(define (generator=? pred . gs)
        (define (all-equal? lst)
                (if (or (null? lst) (null? (cdr lst)))
                    #t
                    (if (equal? (car lst) (cadr lst))
                        (all-equal? (cdr lst))
                        #f)))
        (let loop ((vs (map (lambda (g) (g)) gs)))
             (if (any eof-object? vs)
                 #t
                 (if (all-equal? (map pred vs))
                     (loop (map (lambda (g) (g)) gs))
                     #f))))


;; generator-unfold 
(define (generator-unfold g unfold . args)  ; not sure about this one, though I basically copied the definition given
        (apply unfold (append (list eof-object? (lambda (x) x) (lambda (x) (g)) (g)) args)))



;; generator-let* (copied from spec)
(define-syntax generator-let*  ; definition taken directly from the spec
  (syntax-rules ()
    ((_ () body body2 ...) (begin body body2 ...))
    ((_ ((var gen-expr) more-bindings ...) . body)
     (let ((var gen-expr))
       (if (eof-object? var)
         var
         (generator-let* (more-bindings ...) . body))))
    ((_ (( gen-expr ) more-bindings ...) . body)
     (let ((var gen-expr))
       (if (eof-object? var)
         var
         (generator-let* (more-bindings ...) . body))))))


; do-generator
(define-syntax do-generator
  (syntax-rules ()
    ((_ (var gexpr) body ...) (generator-for-each (lambda (var) body ...) gexpr))))




