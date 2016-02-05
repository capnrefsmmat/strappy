#lang scribble/manual

@require[racket/sandbox
         scribble/eval]

@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'typed/racket)))

@interaction-eval[#:eval my-evaluator
                  (require "empirical-dist.rkt" "distributions.rkt"
                           math/distributions)]

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