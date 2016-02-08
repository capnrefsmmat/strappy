#lang strappy

;; A silly example: finding the confidence intervals for the endpoints of
;; a confidence interval. And then the CIs for the endpoints of those
;; intervals, and so on.

;; We'll be estimating the mean of n samples of a standard normal.
(define n 10)
(define X (n-iid (normal-dist) n))

;; Return the CIs for a sample of means (or CI endpoints).
(define-statistic (ci-for samp)
  (list
   (quantile 0.025 < samp)
   (quantile 0.975 < samp)))

(ci-for (sample (mean X) 100))

;; Take a sample of CIs and 'which', an accessor for which endpoint we want.
(define-statistic (endpoint-ci cis which)
  (ci-for (map which cis)))

;; The CI for the lower endpoint of the CI, based on 100 samples.
(endpoint-ci (sample (ci-for (n-iid (mean X) 100))
		     100)
	     first)

;; Similarly, for the upper endpoint
(endpoint-ci (sample (ci-for (n-iid (mean X) 100))
		     100)
	     last)
