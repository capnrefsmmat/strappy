#lang racket/base

#|  strappy: a Racket language for teaching statistics
    Copyright (C) 2016 Alex Reinhart <alex@refsmmat.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(provide define-statistic permuted)

(require math/distributions
         racket/list)

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
