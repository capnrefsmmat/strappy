#lang racket/base

(provide pi)

(require "statistic.rkt"
         racket/math
         math/statistics)

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
(statistify dexpt expt)
(statistify dexp exp)
(statistify dlog log)

(statistify dsin sin)
(statistify dcos cos)
(statistify dtan tan)
(statistify dasin asin)
(statistify dacos acos)
(nary-statistify datan atan)

(statistify dsqr sqr)

(statistify dsinh sinh)
(statistify dcosh cosh)
(statistify dtanh tanh)

(statistify dorder-of-magnitude order-of-magnitude)

;;; Basic statistics
(nary-statistify dmean mean)
