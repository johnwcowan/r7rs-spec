; Copyright (c) 2005 Sebastian Egner and Jens Axel S{\o}gaard.
; 
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; ``Software''), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
; 
; -----------------------------------------------------------------------
;
; Defining the R5RS comparison predicates in terms of the compare SRFI
; Sebastian.Egner@philips.com

; (load "compare.scm")

(define (=  z1 z2 . zs) (apply chain=?  number-compare z1 z2 zs))
(define (<  x1 x2 . xs) (apply chain<?  real-compare   x1 x2 xs))
(define (>  x1 x2 . xs) (apply chain>?  real-compare   x1 x2 xs))
(define (<= x1 x2 . xs) (apply chain<=? real-compare   x1 x2 xs))
(define (>= x1 x2 . xs) (apply chain>=? real-compare   x1 x2 xs))

(define char=?       (=?  char-compare))
(define char<?       (<?  char-compare))
(define char>?       (>?  char-compare))
(define char<=?      (<=? char-compare))
(define char>=?      (>=? char-compare))

(define char-ci=?    (=?  char-compare-ci))
(define char-ci<?    (<?  char-compare-ci))
(define char-ci>?    (>?  char-compare-ci))
(define char-ci<=?   (<=? char-compare-ci))
(define char-ci>=?   (>=? char-compare-ci))

(define string=?     (=?  string-compare))
(define string<?     (<?  string-compare))
(define string>?     (>?  string-compare))
(define string<=?    (<=? string-compare))
(define string>=?    (>=? string-compare))

(define string-ci=?  (=?  string-compare-ci))
(define string-ci<?  (<?  string-compare-ci))
(define string-ci>?  (>?  string-compare-ci))
(define string-ci<=? (<=? string-compare-ci))
(define string-ci>=? (>=? string-compare-ci))

