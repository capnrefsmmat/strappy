#lang racket/base

#|  deriv-lang-test.ss: Test suite for deriv-lang.ss.
    Copyright (C) 2007 Will M. Farr <farr@mit.edu>
    Ported to modern Racket by Alex Reinhart.

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

(module deriv-lang-test "deriv-lang.rkt"
  (require rackunit)
  
  (provide deriv-lang-test-suite)
  
  (define-simple-check (check-close? eps a b)
    (< (abs (- a b)) eps))
  
  (define deriv-lang-test-suite
    (test-suite
     "deriv-lang.ss test suite"
     (test-case
      "testing add, negate, sub, mul, invert, negate, and partial"
      (let ([f (lambda (x y)
		 (+ (* 3 (- x))
		    (* (/ y) x)
		    (* x x x)
		    (/ y x y)
		    (- 3 (* x y))))])
        (check-equal? (f 2 3) 1/6)
        (check-equal? (((partial 0) f) 2 3) 73/12)
        (check-equal? (((partial 1) f) 2 3) -20/9)))
     (test-case
      "testing =, <, >, <=, >=, and sin and cos"
      (let ((f (lambda (x) (if (= x 3) (cos x) (sin x)))))
        (check-equal? ((D f) 3) (- (sin 3)))
        (check-equal? ((D f) 4) (cos 4)))
      (let ((f (lambda (x) (if (< x 3) (cos x) (sin x)))))
        (check-equal? ((D f) 2.9) (- (sin 2.9)))
        (check-equal? ((D f) 4) (cos 4)))
      (let ((f (lambda (x) (if (> x 3) (cos x) (sin x)))))
        (check-equal? ((D f) 4) (- (sin 4)))
        (check-equal? ((D f) 3) (cos 3)))
      (let ((f (lambda (x) (if (<= x 3) (cos x) (sin x)))))
        (check-equal? ((D f) 3) (- (sin 3)))
        (check-equal? ((D f) 4) (cos 4)))
      (let ((f (lambda (x) (if (>= x 3) (cos x) (sin x)))))
        (check-equal? ((D f) 3) (- (sin 3)))
        (check-equal? ((D f) 2) (cos 2))))
     (test-case
      "testing exp and log"
      (let ((f (lambda (x) (exp (log x)))))
        (check-close? 1e-8 ((D f) (random)) 1.0)))
     (test-case
      "testing trig and inverse trig"
      (let ((x (random)))
        (check-close? 1e-8 ((D sin) x) (/ ((D asin) (sin x))))
        (check-close? 1e-8 ((D cos) x) (/ ((D acos) (cos x))))
        (check-close? 1e-8 ((D tan) x) (/ ((D atan) (tan x)))))
      (let ((my-atan2 (lambda (y x) (atan (/ y x))))
            (x (random))
            (y (random)))
        (check-close? 1e-8 (((partial 0) atan) y x) (((partial 0) my-atan2) y x))
        (check-close? 1e-8 (((partial 1) atan) y x) (((partial 1) my-atan2) y x))))
     (test-case
      "testing sqrt and exp"
      (let ((x (random)))
        (check-close? 1e-8 ((D sqrt) x) ((D (lambda (x) (exp (/ (log x) 2)))) x))))
     (test-case
      "testing expt"
      (let* ((my-expt (lambda (a b) (exp (* b (log a)))))
             (x (random))
             (y (random)))
        (check-close? 1e-8 (((partial 0) expt) x y) (((partial 0) my-expt) x y))
        (check-close? 1e-8 (((partial 1) expt) x y) (((partial 1) my-expt) x y))))
     (test-case
      "up to three derivatives of exp"
      (let ((x (random)))
        (check-close? 1e-8 (exp x) ((D exp) x))
        (check-close? 1e-8 (exp x) ((D (D exp)) x))
        (check-close? 1e-8 (exp x) ((D (D (D exp))) x))))
     (test-case
      "random (fixed) polynomial has correct derivatives (up to D^3)."
      (let ((f (lambda (x) (- -10
                              (* 62 (expt x 5))
                              (* -97 (expt x 4))
                              (* 73 (expt x 3))
                              (* 4 (* x x))
                              (* 83 x))))
            (Df (lambda (x) (+ (* -310 (expt x 4))
                               (* 388 (expt x 3))
                               (* -219 (expt x 2))
                               (* -8 x)
                               -83)))
            (DDf (lambda (x) (+ (* -1240 (expt x 3))
                                (* 1164 (* x x))
                                (* -438 x)
                                -8)))
            (DDDf (lambda (x) (+ (* -3720 (* x x))
                                 (* 2328 x)
                                 -438)))
            (x (random)))
        (check-close? 1e-8 (Df x) ((D f) x))
        (check-close? 1e-8 (DDf x) ((D (D f)) x))
        (check-close? 1e-8 (DDDf x) ((D (D (D f))) x))))
     (test-case
      "Higher-order derivative test (from Barak A. Pearlmutter)"
      (check-close? 1e-8
                    ((D (lambda (x) ((D (lambda (y) (+ x y))) 1))) 1)
                    0)))))

(require 'deriv-lang-test)
(provide (all-from-out 'deriv-lang-test))
