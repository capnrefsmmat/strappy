#lang racket/base

#|  deriv-lang.ss: MzScheme language for automatic differentiation.
    Copyright (C) 2007 Will M. Farr <farr@mit.edu>
    Ported to Racket by Alex Reinhart.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(require racket/match
	 racket/math
	 racket/contract)

(define (deriv-number? obj)
  (or (number? obj) (deriv? obj)))

(define-struct deriv
  (tag x dx))

(define (arity-at-least/c n)
  (flat-named-contract
   (format "arity-at-least-~a" n)
   (lambda (proc)
     (let ([a (procedure-arity proc)])
       (cond
	[(integer? a) (>= a n)]
	[(arity-at-least? a) (<= (arity-at-least-value a) n)]
	[else (ormap (lambda (a)
		       (or (and (integer? a) (>= a n))
			   (and (arity-at-least? a) (<= (arity-at-least-value a) n))))
		     a)])))))

(provide (rename-out (deriv-number? number?)))

;; The ->r contract constructor no longer exists in Racket, and I can't find
;; documentation on its original purpose, so I haven't reconstructed the
;; contract in modern Racket yet. -ar
(provide partial)

(provide/contract
 (D (-> (-> deriv-number? deriv-number?) (-> deriv-number? deriv-number?)))
 ;; (partial (->r ((i natural-number/c))
 ;; 	       (-> (and/c (unconstrained-domain-> deriv-number?)
 ;; 			  (arity-at-least/c (+ i 1)))
 ;; 		   (and/c (unconstrained-domain-> deriv-number?)
 ;; 			  (arity-at-least/c (+ i 1))))))
 (gradient (-> (-> (vectorof deriv-number?) deriv-number?)
	       (-> (vectorof deriv-number?) (vectorof deriv-number?))))
 (jacobian (-> (-> (vectorof deriv-number?) (vectorof deriv-number?))
	       (-> (vectorof deriv-number?) (vectorof (vectorof deriv-number?)))))
 (struct deriv ((tag natural-number/c)
		(x deriv-number?)
		(dx deriv-number?)))
 (rename my-+ + (->* () #:rest (listof deriv-number?) deriv-number?))
 (rename my-- - (->* (deriv-number?) #:rest (listof deriv-number?) deriv-number?))
 (rename my-* * (->* () #:rest (listof deriv-number?) deriv-number?))
 (rename my-/ / (->* (deriv-number?) #:rest (listof deriv-number?) deriv-number?))
 (rename my-= = (->* (deriv-number? deriv-number?) (listof deriv-number?) boolean?))
 (rename my-< < (->* (deriv-number? deriv-number?) (listof deriv-number?) boolean?))
 (rename my-> > (->* (deriv-number? deriv-number?) (listof deriv-number?) boolean?))
 (rename my-<= <= (->* (deriv-number? deriv-number?) (listof deriv-number?) boolean?))
 (rename my->= >= (->* (deriv-number? deriv-number?) (listof deriv-number?) boolean?))
 (rename my-zero? zero? (-> deriv-number? boolean?))
 (rename my-positive? positive? (-> deriv-number? boolean?))
 (rename my-negative? negative? (-> deriv-number? boolean?))
 (rename my-abs abs (-> deriv-number? deriv-number?))
 (rename my-exp exp (-> deriv-number? deriv-number?))
 (rename my-log log (-> deriv-number? deriv-number?))
 (rename my-sin sin (-> deriv-number? deriv-number?))
 (rename my-cos cos (-> deriv-number? deriv-number?))
 (rename my-tan tan (-> deriv-number? deriv-number?))
 (rename my-asin asin (-> deriv-number? deriv-number?))
 (rename my-acos acos (-> deriv-number? deriv-number?))
 (rename my-atan atan (case->
		       (-> deriv-number? deriv-number?)
		       (-> deriv-number? deriv-number? deriv-number?)))
 (rename my-sqrt sqrt (-> deriv-number? deriv-number?))
 (rename my-sqr sqr (-> deriv-number? deriv-number?))
 (rename my-expt expt (-> deriv-number? deriv-number? deriv-number?)))

;; We need to use a channel here because we want generating id's for tags to be
;; thread-safe.
(define tag-channel (make-channel))

(thread
 (lambda ()
   (let loop ([i 0])
     (channel-put tag-channel i)
     (loop (+ i 1)))))

(define (next-tag)
  (channel-get tag-channel))


(define (extract-derivative tag x)
  (if (not (deriv? x))
      0
      (let ([x-tag (deriv-tag x)])
        (cond
	 [(= x-tag tag)
	  (deriv-dx x)]
	 [(> x-tag tag)
	  (make-deriv x-tag
		      (extract-derivative tag (deriv-x x))
		      (extract-derivative tag (deriv-dx x)))]
	 [else
	  (raise-mismatch-error 'extract-derivative "tag not found!" tag)]))))

(define ((D f) x)
  (let* ([tag (next-tag)]
	 [x (make-deriv tag x 1)]
	 [result (f x)])
    (extract-derivative tag result)))

(define (tag-ith list tag i)
  (cond
   [(null? list) (raise-mismatch-error 'tag-ith "list too short" list)]
   [(= i 0) (cons (make-deriv tag (car list) 1)
		  (cdr list))]
   [else (cons (car list) (tag-ith (cdr list) tag (- i 1)))]))

(define ((partial i) f)
  (lambda args
    (let* ([tag (next-tag)]
	   [tagged-args (tag-ith args tag i)]
	   [result (apply f tagged-args)])
      (extract-derivative tag result))))

(define (vtag-ith vec tag i)
  (define len (vector-length vec))
  (for/vector
      #:length len
      ([x vec] [j (in-range len)])
    (if (= i j)
	(make-deriv tag x 1)
	x)))

(define ((gradient f) v)
  (define len (vector-length v))
  (for/vector
      #:length len
      ([i (in-range len)])
      (let* ([tag (next-tag)]
	     [tagged-arg (vtag-ith v tag i)]
	     [result (f tagged-arg)])
	(extract-derivative tag result))))

(define matrix-rows vector-length)
(define (matrix-cols m)
  (vector-length (vector-ref m 0)))
(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j))

(define (matrix-transpose mat)
  (let ([n (matrix-rows mat)]
	[m (matrix-cols mat)])
    (for/vector #:length m
	([i (in-range m)])
	(for/vector #:length n
	    ([j (in-range n)])
	    (matrix-ref mat j i)))))

(define ((jacobian f) v)
  (matrix-transpose
   (for/vector #:length (vector-length v)
       ([i (in-range (vector-length v))])
       (let* ([tag (next-tag)]
	      [tagged-arg (vtag-ith v tag i)]
	      [result-v (f tagged-arg)])
	 (for/vector #:length (vector-length result-v)
	     ([res result-v])
	     (extract-derivative tag res))))))

;; Watch out for code explosion---lift1 and lift2 need to be macros
;; in order to properly handle recursive definitions, but take care
;; not to use any expressions inside the lift1 and lift2 syntax.
;; If you do, they will be duplicated several times.
(define-syntax lift1
  (syntax-rules ()
    ((lift1 ndf f df)
     (match-lambda
       ((struct deriv (dx x xbar))
        (make-deriv dx (f x) (mul (df x) xbar)))
       (x (ndf x))))))

(define-syntax lift2
  (syntax-rules ()
    ((lift2 ndf f dfdx dfdy)
     (lambda (xx yy)
       (match xx
         ((struct deriv (dx x xbar))
          (match yy
            ((struct deriv (dy y ybar))
             (cond
               ((= dx dy)
                (make-deriv dx (f x y) (add (mul (dfdx x y) xbar)
                                            (mul (dfdy x y) ybar))))
               ((> dx dy)
                (make-deriv dx (f x yy) (mul (dfdx x yy) xbar)))
               (else 
                (make-deriv dy (f xx y) (mul (dfdy xx y) ybar)))))
            (y (make-deriv dx (f x y) (mul (dfdx x y) xbar)))))
         (x
          (match yy
            ((struct deriv (dy y ybar))
             (make-deriv dy (f x y) (mul (dfdy x y) ybar)))
            (y (ndf x y)))))))))

(define my-+
  (case-lambda
    [() 0]
    [(x) x]
    [(x y) (add x y)]
    [(x y . zs)
     (apply my-+ (add x y) zs)]))

(define my--
  (case-lambda
    [(x) (negate x)]
    [(x y) (sub x y)]
    [(x . ys)
     (sub x (apply my-+ ys))]))

(define my-*
  (case-lambda
    [() 1]
    [(x) x]
    [(x y) (mul x y)]
    [(x y . zs)
     (apply my-* (mul x y) zs)]))

(define my-/
  (case-lambda
    [(x) (invert x)]
    [(x y) (div x y)]
    [(x . ys)
     (div x (apply my-* ys))]))

(define add
  (let ([dadddarg (lambda (x y) 1)])
    (lift2 + add dadddarg dadddarg)))

(define negate
  (let ([Dneg (lambda (x) -1)])
    (lift1 - negate Dneg)))

(define sub
  (let ([dsubdx (lambda (x y) 1)]
	[dsubdy (lambda (x y) -1)])
    (lift2 - sub dsubdx dsubdy)))

(define mul
  (let ([dmuldx (lambda (x y) y)]
	[dmuldy (lambda (x y) x)])
    (lift2 * mul dmuldx dmuldy)))

(define invert
  (let ([Dinvert (lambda (x)
		   (invert (negate (mul x x))))])
    (lift1 / invert Dinvert)))

(define div
  (let ([ddivdx (lambda (x y) (invert y))]
	[ddivdy (lambda (x y) (negate (div x (mul y y))))])
    (lift2 / div ddivdx ddivdy)))

(define-syntax lift-comp
  (syntax-rules ()
    ((lift-comp ndcomp comp)
     (lambda (xx yy)
       (match xx
         [(struct deriv (_ x _))
	  (comp x yy)]
         [x (match yy
	      ((struct deriv (_ y _))
	       (comp x y))
	      (y (ndcomp x y)))])))))

(define =2 (lift-comp = =2))
(define my-=
  (case-lambda
    [(x y) (=2 x y)]
    [(x y . zs)
     (and (=2 x y) (apply my-= y zs))]))

(define <2 (lift-comp < <2))
(define my-<
  (case-lambda
    [(x y) (<2 x y)]
    [(x y . zs)
     (and (<2 x y) (apply my-< y zs))]))

(define >2 (lift-comp > >2))
(define my->
  (case-lambda
    [(x y) (>2 x y)]
    [(x y . zs) (and (>2 x y) (apply my-> y zs))]))

(define <=2 (lift-comp <= <=2))
(define my-<=
  (case-lambda
    [(x y) (<=2 x y)]
    [(x y . zs) (and (<=2 x y) (apply my-<= y zs))]))

(define >=2 (lift-comp >= >=2))
(define my->=
  (case-lambda
    [(x y) (>=2 x y)]
    [(x y . zs) (and (>=2 x y) (apply my->= y zs))]))

(define-syntax lift-pred
  (syntax-rules ()
    ((lift-pred ndp p)
     (lambda (x)
       (match x
         [(struct deriv (_ x _))
	  (p x)]
         [x (ndp x)])))))

(define my-zero? (lift-pred zero? my-zero?))
(define my-positive? (lift-pred positive? my-positive?))
(define my-negative? (lift-pred negative? my-negative?))
(define my-odd? (lift-pred odd? my-odd?))
(define my-even? (lift-pred even? my-even?))

(define my-abs
  (let ([Dabs (lambda (x)
		(when (my-zero? x)
		  (raise-mismatch-error 'abs "no derivative at zero" x))
		(if (my-negative? x)
		    -1
		    1))])
    (lift1 abs my-abs Dabs)))

(define my-exp (lift1 exp my-exp my-exp))
(define my-log (lift1 log my-log invert))
(define my-sin (lift1 sin my-sin my-cos))
(define my-cos
  (let ([Dcos (lambda (x) (negate (my-sin x)))])
    (lift1 cos my-cos Dcos)))
(define my-tan
  (let ([Dtan (lambda (x)
		(let ((cos-x (my-cos x)))
		  (invert (mul cos-x cos-x))))])
    (lift1 tan my-tan Dtan)))

(define my-asin
  (let ([Dasin (lambda (x) (invert (my-sqrt (sub 1 (mul x x)))))])
    (lift1 asin my-asin Dasin)))
(define my-acos
  (let ([Dacos (lambda (x) (negate (invert (my-sqrt (sub 1 (mul x x))))))])
    (lift1 acos my-acos Dacos)))

(define atan1
  (let ([Datan1 (lambda (x) (invert (add 1 (mul x x))))])
    (lift1 atan atan1 Datan1)))
(define atan2
  (let ([datan2dy (lambda (y x) (div x (add (mul x x) (mul y y))))]
	[datan2dx (lambda (y x) (negate (div y (add (mul x x) (mul y y)))))])
    (lift2 atan atan2 datan2dy datan2dx)))
(define my-atan
  (case-lambda
    [(x) (atan1 x)]
    [(y x) (atan2 y x)]))

(define my-sqrt
  (let ([Dsqrt (lambda (x) (div 1/2 (my-sqrt x)))])
    (lift1 sqrt my-sqrt Dsqrt)))

(define my-sqr
  (let ([Dsqr (lambda (x) (mul 2 x))])
    (lift1 sqr my-sqr Dsqr)))

(define my-expt
  (let ([dexptdx (lambda (x y) (mul y (my-expt x (sub y 1))))]
	[dexptdy (lambda (x y) (mul (my-log x) (my-expt x y)))])
    (lift2 expt my-expt dexptdx dexptdy)))
