#lang racket/base

#|  strappy: a Racket language for teaching statistics
    Copyright (C) 2016 Alex Reinhart <alex@refsmmat.com>

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

(require "statistic.rkt"
         "autodiff/deriv.rkt"
         math/statistics)

;; Provide derivative operators from autodiff.
(provide D gradient jacobian (struct-out deriv))

(define-syntax-rule (statistify newname oldname)
  (begin
    (define-statistic (newname arg)
      (oldname arg))
    (provide (rename-out [newname oldname]))))

;; For variable-arg generic math functions, make it easy to define a statistic
;; that just feeds the arguments to the original functions. Do some renaming so
;; the definitions don't infect other functions in this file.
(define-syntax-rule (nary-statistify newname oldname)
  (begin
    (define-statistic (newname num . nums)
      (apply oldname num nums))
    (provide (rename-out [newname oldname]))))

;;; Arithmetic and mathematical functions

(nary-statistify d+ +)
(nary-statistify d- -)
(nary-statistify d* *)
(nary-statistify d/ /)

(statistify dabs abs)
(nary-statistify dmax max)
(nary-statistify dmin min)
(statistify dfloor floor)
(statistify dceiling ceiling)

(nary-statistify d= =)
(nary-statistify d< <)
(nary-statistify d<= <=)
(nary-statistify d> >)
(nary-statistify d>= >=)

(statistify dsqrt sqrt)
(nary-statistify dexpt expt)
(statistify dexp exp)
(statistify dlog log)

(statistify dsin sin)
(statistify dcos cos)
(statistify dtan tan)
(statistify dasin asin)
(statistify dacos acos)
(nary-statistify datan atan)

(statistify dsqr sqr)

;;; Basic statistics
(nary-statistify dmean mean)
(nary-statistify dquantile quantile)

;; I don't think I can write a macro that defines a function to accept
;; arbitrary keyword arguments; instead, declare both cases as separate
;; functions unified by a single statistic.
(define-statistic (variance-bias xs ws)
  (variance xs ws #:bias #t))

(define-statistic (variance-nobias xs ws)
  (variance xs ws))

(define (variance-wrap xs [ws #f] #:bias [bias #f])
  (if bias
      (variance-bias xs ws)
      (variance-nobias xs ws)))

(provide (rename-out [variance-wrap variance]))

(define-statistic (stddev-bias xs ws)
  (stddev xs ws #:bias #t))

(define-statistic (stddev-nobias xs ws)
  (stddev xs ws))

(define (stddev-wrap xs [ws #f] #:bias [bias #f])
  (if bias
      (stddev-bias xs ws)
      (stddev-nobias xs ws)))

(provide (rename-out [stddev-wrap stddev]))
