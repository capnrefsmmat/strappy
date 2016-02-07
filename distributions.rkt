#lang typed/racket

;; Provide tools for working with distribution objects, including the basic
;; methods from math/distributions. Provide also the basic distribution
;; constructors.
(provide n-iid pdf sample cdf inv-cdf
         beta-dist beta-dist-alpha beta-dist-beta
         cauchy-dist cauchy-dist-mode cauchy-dist-scale
         delta-dist delta-dist-mean
         exponential-dist exponential-dist-mean
         gamma-dist gamma-dist-shape gamma-dist-scale
         logistic-dist logistic-dist-mean logistic-dist-scale
         normal-dist normal-dist-mean normal-dist-stddev
         triangle-dist triangle-dist-min triangle-dist-max triangle-dist-mode
         truncated-dist truncated-dist-original truncated-dist-min truncated-dist-max
         uniform-dist uniform-dist-min uniform-dist-max)

(require math/distributions)
(require racket/flonum)

(define-type Multivariate-Dist (distribution (Vectorof Real) (Vectorof Real)))

(: n-iid (-> (distribution Real Real) Integer Multivariate-Dist))
(define (n-iid dist n)

  ;; The pdf of a joint distribution of independent RVs is just the product
  ;; of the marginal pdfs.
  (: pdf (->* ((Vectorof Real)) (Any) Flonum))
  (define (pdf query [log? #f])
    (when (not (= (vector-length query) n))
      (raise-arguments-error 'pdf
                             "Query vector's dimensions don't match distribution's dimension"
                             "query" (vector-length query)
                             "distribution" n))

    (: marginal (-> Real Flonum))
    (define (marginal query)
      ((distribution-pdf dist) query))

    (: marginals (Listof Flonum))
    (define marginals (map marginal (vector->list query)))

    (if log?
        (foldl fl+ 0.0 marginals)
        (foldl fl* 1.0 marginals)))

  (: sample : (Sample (Vectorof Real)))
  (define (sample [samples #f])
    (if samples
        (for/list : [Listof (Vectorof Real)]
                   ([i (in-range 0 samples)])
          (list->vector ((distribution-sample dist) n)))
        (list->vector ((distribution-sample dist) n))))

  (distribution pdf sample))

(module+ test
  (require typed/rackunit)

  (define foo (n-iid (normal-dist) 3))

  (check-equal? ((distribution-pdf foo) #(0 0 0)) (expt (pdf (normal-dist) 0) 3))
  (check-equal? ((distribution-pdf foo) #(0 0 0) #t) (* 3 (pdf (normal-dist) 0)))

  (check-equal? (vector-length ((distribution-sample foo))) 3)
  (check-equal? (vector-length (first ((distribution-sample foo) 2))) 3)

  (check-exn exn:fail? (lambda () ((distribution-pdf foo) #(0 0 0 0)))))
