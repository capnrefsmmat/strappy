#lang strappy

;; If we use significance as a filter, then with low power we'll tend to
;; overestimate effect sizes. To demonstrate, we'll do a simple z test,
;; with p = 0.05 (|z| >= 1.96) cutoff, of whether the mean of some data
;; is zero. We'll assume the standard deviation is known, although the
;; results would be similar even without it.

(require plot)

(define critical-value 1.96)
(define (significant? z)
  (>= (abs z) critical-value))

(define num-sims 1000)

;; The alternative hypothesis is that X has mean 2 and stddev 2.
(define X (normal-dist 1 2))

(define (effect-and-power n)
  (define samples (sample (n-iid X n) num-sims))

  (define sample-means (map mean samples))
  (define z-stats (map (lambda (x) (/ x (/ 2 (sqrt n)))) sample-means))

  (define power (/ (count significant? z-stats)
                   num-sims))

  (define mean-effect
    (mean (for/list ([effect sample-means]
                     [z z-stats]
                     #:when (significant? z))
            effect)))

  (list power mean-effect))

(define results (map effect-and-power (list 5 10 15 20 25 30 40 50 100)))

(parameterize ([plot-x-label "Power"]
               [plot-y-label "Mean effect size"])
  (plot (points results
                #:x-min 0 #:x-max 1
                #:y-min 1 #:y-max 2.5)))
