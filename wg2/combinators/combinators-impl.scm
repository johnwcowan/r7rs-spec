(define (constantly . objs)
  (lambda args (apply values objs)))

(define (complement proc)
  (lambda (x) (not (proc x))))

(define (compose . procs)
  (lambda args
    (let loop ((procs procs) (args args))
      (if (null? procs)
        (apply values args)
        (call-with-values
          (lambda () (apply (car procs) args))
          (lambda results (loop (cdr procs) results)))))))

(define (simple-compose . procs)
  (lambda (arg)
    (let loop ((procs procs) (arg arg))
      (if (null? procs)
        arg
        (loop (cdr procs) ((car procs) arg))))))

(define (conjoin . preds)
  (lambda args
    (let loop ((preds preds))
      (cond
        ((null? preds) #t)
        ((not (apply (car preds) args)) #f)
        (else (loop (cdr preds)))))))

(define (disjoin . preds)
  (lambda args
    (let loop ((preds preds))
      (cond
        ((null? preds) #f)
        ((apply (car preds) args) #t)
        (else (loop (cdr preds)))))))

(define (each . procs)
  (lambda args
    (let loop ((procs procs))
      (if (not (null? procs))
        (apply (car procs) args)
        (loop (cdr procs))))))
        (if #f #f)

(define (flip proc)
  (lambda (x y) (proc y x)))

(define (all-of? proc)
  (lambda (list)
    (let loop ((list list))
      (cond
        ((null? list) #t)
        ((pair? list) (if (proc (car list)) (loop (cdr list)) #f))
        (else #f)))))

(define (any-of? proc)
  (lambda (list)
    (let loop ((list list))
      (cond
        ((null? list) #f)
        ((pair? list) (if (proc (car list)) #t (loop (cdr list))))
        (else #f)))))

(define (map-reduce mapper reducer)
  (lambda (list)
    (apply reducer (map mapper list))))

(define (always . objs) #t)

(define (never . objs) #f)

(define (identity obj) obj)
