#lang strappy

;; A silly example: finding the confidence intervals for the endpoints of
;; a confidence interval. And then the CIs for the endpoints of those
;; intervals, and so on.

;; We'll be estimating the mean of n samples of a standard normal.
(define n 10)
(define X (n-iid (normal-dist) n))

;; The lower endpoint of a 95% CI.
(define (lower-tail dist)
  (quantile 0.025 < dist))

(sample (lower-tail (n-iid (mean X) 100)))

(sample (lower-tail (n-iid (lower-tail (n-iid (mean X) 100)) 100)))
