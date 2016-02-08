#lang racket/base

(require "distributions.rkt"
         "empirical-dist.rkt"
         "statistic.rkt"
         "basic-statistics.rkt"
	 racket/list)

(provide (all-from-out "distributions.rkt"
                       "empirical-dist.rkt"
                       "statistic.rkt"
                       "basic-statistics.rkt")
         #%module-begin #%app #%datum #%top #%top-interaction
         lambda if cond define require parameterize
         list vector count filter map
         first rest second third fourth fifth sixth seventh eighth ninth tenth
	 last)
