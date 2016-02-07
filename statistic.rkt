#lang racket

; XXX: This module isn't typed because function->statistic is hard to type.

(provide define-statistic)

(require math/distributions)

(define (function->statistic fun)
  (define (pdf query [log? #f])
    (raise exn:fail:unsupported))

  (define (arg->sample arg)
    (if (distribution? arg)
        (sample arg)
        arg))

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

(define-syntax-rule (define-statistic (fun args ...) body ...)
  (define fun (function->statistic (lambda (args ...) body ...))))

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
