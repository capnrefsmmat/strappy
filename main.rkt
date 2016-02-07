#lang racket/base

(require "distributions.rkt"
         "empirical-dist.rkt"
         "statistic.rkt"
         "basic-statistics.rkt"
	 racket/list
	 plot)

(provide (all-from-out "distributions.rkt"
                       "empirical-dist.rkt"
                       "statistic.rkt"
                       "basic-statistics.rkt"
		       plot)
         #%module-begin #%app #%datum #%top #%top-interaction
         lambda if cond define parameterize
         list vector count filter map)
