#lang strappy

(require plot)

(define X (normal-dist 1 2))
(define Y (normal-dist 0 2))

(define-statistic (abs-mean-difference xs ys)
  (abs (- (mean xs) (mean ys))))

(define permuted-mean-difference (permuted abs-mean-difference))

;; Calculate the power of the test for a given sample size.
(define (power n)
  (define Xs (n-iid X n))
  (define Ys (n-iid Y n))
  
  (define null-dist (permuted-mean-difference Xs Ys))

  (define critical-value (quantile 0.95 < (sample null-dist 1000)))

  (define alternative-dist (abs-mean-difference Xs Ys))

  (/ (count (lambda (diff) (>= diff critical-value))
            (sample alternative-dist 1000))
     1000))

;; Calculate the power for a few different sample sizes.
(define sizes (list 10 20 50 100))
(define powers (map power sizes))

(parameterize ([plot-x-label "Sample size per group"]
               [plot-y-label "Power"])
  (plot (points (map vector sizes powers)
                #:x-min 0 #:x-max 110
                #:y-min 0 #:y-max 1)))
