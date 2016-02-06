#lang scribble/manual

@require[racket/sandbox
         scribble/eval]

@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'racket)))

@interaction-eval[#:eval my-evaluator
                  (require "empirical-dist.rkt" "distributions.rkt"
                           "statistic.rkt" math/distributions)]

@require[@for-label[strappy
                    racket/base
                    math/distributions]]

@title{strappy: Statistical Concepts in Racket}
@author{Alex Reinhart}

@defmodule[strappy]

@tt{strappy} is a Racket-based teaching language for demonstrating concepts in
basic introductory statistics.

@section{Distributions}

@defproc[(empirical [data (Listof Real)])
         Empirical-Dist]{
 Creates an empirical distribution from the list of provided
 data points. The empirical distribution supports @racket[sample], @racket[cdf],
 @racket[inv-cdf], @racket[pdf], like other common distributions. Samples from
 an empirical distribution are bootstrap samples.

 @examples[#:eval my-evaluator

           (define foo (empirical (list 1 2 2 3 4 5)))
           (sample foo 10)

           (pdf foo 2)

           (cdf foo 3)]
}

@section{Distribution Modifiers}

It's sometimes useful to create new distributions out of combinations of
previous distributions.

@defproc[(n-iid [dist (distribution Real Real)] [n Integer])
         Multivariate-Dist]{
 Create a joint distribution of @racket[n] independent and identically
 distributed random variables, each with the distribution @racket[dist].
 The resulting @racket[Multivariate-Dist] object has @racket[pdf] and
 @racket[sample] methods to evaluate the joint distribution:

 @examples[#:eval my-evaluator
           (define three-normals (n-iid (normal-dist) 3))

           (sample three-normals)

           ((distribution-pdf three-normals) #(0 0 0))]
}

@section{Defining Statistics}

Statistics is all about @emph{statistics}: functions of random variables.
These functions are used to run hypothesis tests, calculate confidence
intervals, estimate uncertainty, and do many other useful things.

@tt{strappy} provides tools to define test statistics and observe their
@emph{sampling distributions}: the distributions of their outputs given their
inputs.

@defform[(define-statistic (name args ...) body ...)]{
 Define a test statistic named @racket[name] with arguments @racket[args]. The
 arguments are available inside @racket[body], a sequence of expressions.

 @racket[define-statistic] behaves in the same way as @racket[define], so test
 statistics are like any other functions in Racket. The only difference is that
 test statistics take random variables as arguments, and return a
 @emph{distribution}, not a single value. For example:

 @examples[#:eval my-evaluator
           (define-statistic (adder a b) (+ a b))
           (sample (adder 1 2))
           (sample (adder 1 (normal-dist)))]

 If called with constant arguments, as in @racket[(adder 1 2)] above, the test
 statistic returns a distribution which always produces the same value: the
 function evaluated with those arguments. If any of the arguments are random
 variables, sampling from the test statistic uses values sampled from those
 variables's distributions.
}
