#lang typed/racket

;; Provide tools for working with distribution objects.
(provide n-iid)

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
          (build-vector n (lambda ([i : Index])
                            ((distribution-sample dist)))))
        (build-vector n (lambda ([i : Index])
                            ((distribution-sample dist))))))

  (distribution pdf sample))

(module+ test
  (require typed/rackunit)

  (define foo (n-iid (normal-dist) 3))

  (check-equal? ((distribution-pdf foo) #(0 0 0)) (expt (pdf (normal-dist) 0) 3))
  (check-equal? ((distribution-pdf foo) #(0 0 0) #t) (* 3 (pdf (normal-dist) 0)))

  (check-exn exn:fail? (lambda () ((distribution-pdf foo) #(0 0 0 0)))))
