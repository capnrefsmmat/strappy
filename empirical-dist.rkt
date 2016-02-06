#lang typed/racket

(provide empirical Empirical-Dist)

(require math/distributions)
(require math/statistics)
(require racket/flonum)
(require racket/promise)

;; In a vector where pred is true for all elements up to a partition point,
;; find that partition point: the index of the first element such that pred
;; is false.
(: partition-point (-> (-> Real Boolean) (Vectorof Real)
		       Integer Integer Integer))
(define (partition-point pred? seq first last)
  (: pp-internal (-> (Vectorof Boolean) Integer Integer Integer))
  (define (pp-internal ys first last)
    (let* ([gap (- last first)]
           [h (+ first (floor (/ gap 2)))])
      (cond
        [(= first last) first]
        [(and (= gap 1) (vector-ref ys h)) last]
        [(and (> gap 1) (vector-ref ys h))
         (pp-internal ys h last)]
        [else (pp-internal ys first h)])))
  (pp-internal (vector-map pred? seq) first last))

;; In a sorted vector, find the first element larger than the query.
(: searchsorted (-> Real (Vectorof Real) Integer))
(define (searchsorted query seq)
  (partition-point (lambda (a) (<= a query))
                   seq 0 (vector-length seq)))

;; We extend the ordered-dist struct to include the data making up the
;; empirical distribution.
(struct (In Out) empirical-dist-struct ordered-dist
	([data : (Vectorof Real)]))

(define-type Empirical-Dist (empirical-dist-struct Real Real))

;; Count the number of times each value occurs in a sequence,
;; returning a hash table of value => count pairs.
(: item-counter (-> (Vectorof Real) (HashTable Real Integer)))
(define (item-counter items)
  (for/fold ([table : (HashTable Real Integer) (hash)])
            ([i items])
    (hash-update table i add1 (lambda () 0))))

;; Create an empirical distribution from a list of data.
(: empirical (-> (Listof Real) Empirical-Dist))
(define (empirical data)
  (define ordered-data
    (vector->immutable-vector (list->vector (sort data <))))

  (define n (vector-length ordered-data))

  (define table (item-counter ordered-data))

  (: cdf (->* (Real) (Any Any) Flonum))
  (define (cdf query [log? #f] [1-p? #f])
    (let* ([loc (searchsorted query ordered-data)]
           [p (real->double-flonum (/ loc n))])
      (cond
        [(and log? 1-p?) (fllog (- 1 p))]
        [log? (fllog p)]
        [1-p? (- 1 p)]
        [else p])))

  (: pdf (->* (Real) (Any) Flonum))
  (define (pdf query [log? #f])
    (let ([c (real->double-flonum (hash-ref table query (lambda () +nan.0)))])
      (if log? (fllog (/ c n)) (/ c n))))

  (: inv-cdf (->* (Real) (Any Any) Real))
  (define (inv-cdf q [log? #f] [1-p? #f])
    (vector-ref ordered-data
                (partition-point (lambda (x) (< (cdf x) q))
                                 ordered-data 0 n)))

  (define min (vector-ref ordered-data 0))
  (define max (vector-ref ordered-data (sub1 n)))
  (define med (delay (median < ordered-data)))

  (define sample (case-lambda
                   [() (vector-ref ordered-data
                        (random n))]
                   [([count : Integer])
                    (build-list
                     count (lambda ([i : Index])
                             (vector-ref ordered-data (random n))))]))

  (empirical-dist-struct pdf sample cdf inv-cdf min max med ordered-data))

(module+ test
  (require typed/rackunit math/distributions)

  ; length 8
  (define foo (empirical (list 0 1 1 2 3 3 3.2 4)))

  (check-equal? (pdf foo 1) 0.25)
  (check-equal? (pdf foo 0) 0.125)
  (check-equal? (pdf foo 1 #t) (log 0.25))

  (check-equal? (cdf foo 0) 0.125)
  (check-equal? (cdf foo 1) 0.375)
  (check-equal? (cdf foo 1.5) 0.375)
  (check-equal? (cdf foo 1.5 #f #t) (- 1.0 0.375))
  (check-equal? (cdf foo 2) 0.5)
  (check-equal? (cdf foo 5) 1.0)
  (check-equal? (cdf foo 5 #t) 0.0)

  (check-equal? (inv-cdf foo 0.375) 1)
  (check-equal? (inv-cdf foo 0.376) 2)
  (check-equal? (inv-cdf foo 1.0) 4))
