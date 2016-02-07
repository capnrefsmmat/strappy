#lang strappy

;; A simple one-sample t test equivalent. We have a sample of 12 data points
;; (from an example in Introduction to the Practice of Statistics) and want to
;; test if the true population mean may be 105.

(define readings (list 91.9 97.8 111.4 122.3 105.4 95.0
                       103.8 99.6 96.6 119.3 104.8 101.7))

;; We take the difference between the sample mean and our postulated hypothesis
;; mean.
(define-statistic (abs-difference-from xs constant)
  (abs (- (mean xs) constant)))

(define true-diff (abs-difference-from readings 105))

;; We take bootstrap samples from the sample distribution and compare them
;; to the hypothesis.
(define bootstrap-distribution
  (abs-difference-from
   (n-iid (empirical-dist readings) 12)
   105))

;; Then we count how many of the bootstrapped means are farther from the sample
;; mean than 105. If this number is large, the sampling variation is high, and
;; 105 is a plausible value for the mean; if the number is small, the sampling
;; variation is low, and we can reject the hypothesis.
(count (lambda (diff) (> diff true-diff)) (sample bootstrap-distribution 100))
