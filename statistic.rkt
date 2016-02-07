#lang racket

(provide define-statistic permuted)

(require math/distributions)

(define (arg->sample arg)
    (if (distribution? arg)
        (sample arg)
        arg))

(define (function->statistic fun)
  (define (pdf query [log? #f])
    (raise exn:fail:unsupported))

  (define ((sample-from-fun args) [multiple #f])
    (define (sample) (map arg->sample args))
    (if multiple
        (for/list ([i (in-range 0 multiple)]) (apply fun (sample)))
        (apply fun (sample))))

  (lambda args
    (unless (procedure-arity-includes? fun (length args))
      (apply raise-arity-error fun (procedure-arity fun) args))

    (if (ormap distribution? args)
        (distribution pdf (sample-from-fun args))
        (apply fun args))))

(define-syntax define-statistic
  (syntax-rules ()
    [(define-statistic (fun args ...) body ...)
     (define fun (function->statistic (lambda (args ...) body ...)))]
    [(define-statistic (fun args ... . more) body ...)
     (define fun (function->statistic (lambda (args ... . more) body ...)))]))

;; Take any statistic of two arguments (must be vectors or lists of data) and
;; return a statistic calculated with the permuted argument data.
(define (permuted fun)
  (define (pdf query [log? #f])
    (raise exn:fail:unsupported))

  (define (shuffle-args xs ys)
    (define len-x (vector-length xs))
    (define shuffled (shuffle (append (vector->list xs) (vector->list ys))))
    (list (list->vector (take shuffled len-x)) (list->vector (drop shuffled len-x))))

  (define ((sample-from-shuffled xs ys) [multiple #f])
    (define (shuffled-sample) (shuffle-args (arg->sample xs)
					    (arg->sample ys)))
    (if multiple
	(for/list ([i (in-range 0 multiple)]) (apply fun (shuffled-sample)))
	(apply fun (shuffled-sample))))

  (lambda (xs ys)
    (if (or (distribution? xs) (distribution? ys))
	(distribution pdf (sample-from-shuffled xs ys))
	(fun xs ys))))

(module+ test
  (require rackunit)

  (define-statistic (foo a b)
    (+ a b))

  (check-exn exn:fail? (lambda () (foo 1)))
  (check-exn exn:fail? (lambda () (pdf (foo 1 2))))
  (check-equal? (foo 1 2) 3)

  (define-statistic (multi-foo a b)
    (define c (* 2 a))
    (+ b c))

  (check-equal? (multi-foo 2 3) 7)

  (define-statistic (foo-rest arg . args)
    (apply + arg args))

  (check-equal? (foo-rest 2 3 4) 9))
