#! /usr/local/bin/csi -script

(cond-expand
 (chibi (import (srfi 95)))
 (else))

(define (remove pred ls)
  (let lp ((ls ls) (res '()))
    (cond ((null? ls) (reverse res))
          ((pred (car ls)) (lp (cdr ls) res))
          (else (lp (cdr ls) (cons (car ls) res))))))

(define (every pred ls)
  (or (null? ls)
      (and (pair? ls)
           (pred (car ls))
           (every pred (cdr ls)))))

(define (union/eq a b)
  (cond ((null? a) b)
        ((memq (car a) b) (union/eq (cdr a) b))
        (else (union/eq (cdr a) (cons (car a) b)))))

(define (join-candidates ranked res)
  (let lp ((ls ranked) (res res))
    (cond ((null? ls) res)
          ((pair? (car ls)) (lp (cdr ls) (union/eq (car ls) res)))
          (else (lp (cdr ls) (union/eq (list (car ls)) res))))))

(define (extract-candidates votes)
  (let lp ((ls votes) (res '()))
    (if (null? ls)
        res
        (lp (cdr ls) (join-candidates (cdar ls) res)))))

(define (vote-preferred? a b ls)
  (and (pair? ls)
       (let ((tmp (if (symbol? (car ls)) (list (car ls)) (car ls))))
         (cond ((memq a tmp) (not (memq b tmp)))
               ((memq b tmp) #f)
               (else (vote-preferred? a b (cdr ls)))))))

(define (tally-vote a b votes)
  (let lp ((ls votes) (res 0))
    (if (null? ls)
        res
        (lp (cdr ls) (if (vote-preferred? a b (cdar ls)) (+ res 1) res)))))

(define (tally-votes votes)
  (let ((candidates (extract-candidates votes)))
    (let lp1 ((ls1 candidates) (res '()))
      (if (null? ls1)
          res
          (let lp2 ((ls2 candidates) (res res))
            (cond
             ((null? ls2)
              (lp1 (cdr ls1) res))
             ((eq? (car ls1) (car ls2))
              (lp2 (cdr ls2) res))
             (else
              (lp2 (cdr ls2)
                   (cons (cons (cons (car ls1) (car ls2))
                               (tally-vote (car ls1) (car ls2) votes))
                         res)))))))))

(define (pair-score pair pairs)
  (cond ((assoc pair pairs) => cdr) (else 0)))

(define (sort-pairs pairs)
  ;; requires SRFI-98 compatible `sort': (sort ls less?)
  (sort pairs
        (lambda (a b)
          (or (> (cdr a) (cdr b))
              (and (= (cdr a) (cdr b))
                   (let ((a^-1 (pair-score (cons (cdar a) (caar a)) pairs))
                         (b^-1 (pair-score (cons (cdar b) (caar b)) pairs)))
                     (< a^-1 b^-1)))))))

(define (insert-edge a b graph)
  (let lp ((ls graph) (rev '()))
    (cond
     ((null? ls)
      (cons (list a b) graph))
     ((equal? a (caar ls))
      (if (member b (cdar ls))
          graph
          (append (reverse rev)
                  (cons (cons (caar ls) (cons b (cdar ls))) (cdr ls)))))
     (else
      (lp (cdr ls) (cons (car ls) rev))))))

(define (graph-ref graph a)
  (cond ((assoc a graph) => cdr) (else '())))

;; can a be reached from b with the given graph?
(define (graph-reachable? a b graph)
  (let lp ((ls (graph-ref graph b))
           (seen '()))
    (cond
     ((null? ls) #f)
     ((equal? a (car ls)) #t)
     (else
      (let* ((seen (cons (car ls) seen))
             (new (remove (lambda (x) (member x seen))
                          (graph-ref graph (car ls)))))
        (lp (append new (cdr ls)) seen))))))

(define (lock-pairs pairs)
  (let lp ((ls pairs) (graph '()))
    (cond
     ((null? ls)
      graph)
     ((graph-reachable? (caar ls) (cdar ls) graph)
      (lp (cdr ls) graph))
     (else
      (lp (cdr ls) (insert-edge (caar ls) (cdar ls) graph))))))

(define (topological-sort graph)
  (let visit ((ls graph) (seen '()) (res '()) (return (lambda (seen res) res)))
    (cond
     ((null? ls)
      (return seen res))
     ((member (car (car ls)) seen)
      (visit (cdr ls) seen res return))
     ((member (car (car ls)) res)
      (visit (cdr ls) seen res return))
     (else
      (let scan-deps ((deps (cdr (car ls)))
                      (seen (cons (car (car ls)) seen))
                      (res res))
        (cond
         ((null? deps)
          (visit (cdr ls) seen (cons (car (car ls)) res) return))
         ((member (car deps) seen)
          (scan-deps (cdr deps) seen res))
         ((member (car deps) res)
          (scan-deps (cdr deps) seen res))
         ((assoc (car deps) graph)
          => (lambda (vertices)
               (visit (list vertices)
                      seen
                      res
                      (lambda (seen res)
                        (scan-deps (cdr deps) seen res)))))
         (else
          (scan-deps (cdr deps) seen (cons (car deps) res)))))))))

(define (rank-votes votes)
  (topological-sort (lock-pairs (map car (sort-pairs (tally-votes votes))))))

;;; sample votes
;; (define votes
;;   '((member-a (A) (A-LITE) (SRFI) (B) (R5RS))
;;     (member-b (B) (SRFI) (R5RS) (A-LITE) (A))
;;     (member-c (SRFI) (A-LITE) (R5RS) (A B))
;;     (member-d (R5RS) (A B SRFI A-LITE))
;;     (member-e (A-LITE) (B) (A) (SRFI) (R5RS))
;;     (member-f (A) (B) (A-LITE) (SRFI) (R5RS))
;;     (member-g (B) (A-LITE) (A) (SRFI) (R5RS))
;;     (member-h (A-LITE) (SRFI) (B) (A) (R5RS))
;;     ))

(define votes (read))

(let ((ranked (rank-votes votes)))
  (cond
   ((pair? ranked)
    (let ((winner (car ranked)))
      (write ranked)
      (newline)
      (write (map (lambda (loser)
                    (list (tally-vote winner loser votes)
                          (tally-vote loser winner votes)))
                  (cdr ranked)))
      (newline)))
   ((and (pair? votes)
         (every (lambda (v) (equal? (cdr v) (cdar votes))) (cdr votes)))
    ;; unanimous - only one option found
    (write (cdar votes))
    (newline)
    (write (list))
    (newline))
   (else
    (display ";; no ranking found for: " (current-error-port))
    (write votes (current-error-port))
    (newline (current-error-port)))))
