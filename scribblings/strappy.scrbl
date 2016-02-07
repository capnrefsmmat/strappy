#lang scribble/manual

@require[racket/sandbox
         scribble/eval]

@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'strappy)))

@require[@for-label[strappy
                    math/distributions]]

@require[@for-label[@only-in[racket/base
                	     lambda if cond define list]]]

@title{strappy: Statistical Concepts in Racket}
@author{Alex Reinhart}

@defmodule[strappy #:lang]

@tt{strappy} is a Racket-based teaching language for demonstrating concepts in
basic introductory statistics.

To give you the flavor of @tt{strappy}, let's start with an example. Suppose 12
commercially-available radon detectors are placed in a chamber containing
exactly 105 picocuries per liter of gas. The 12 detectors give different
readings; we'd like to test whether the average detector is @emph{biased} in
some way---that is, if the mean of the distribution of detector readings is
different from 105 picocuries per liter.

@margin-note{This example stolen from @emph{Introduction to the Practice of
Statistics} and reworked to use bootstrapping.}


@examples[#:eval my-evaluator
          (define readings (list 91.9 97.8 111.4 122.3 105.4 95.0
                                 103.8 99.6 96.6 119.3 104.8 101.7))

	  (mean readings)]

So the average isn't exactly 105. Is this difference plausible sampling error,
or is it big enough that we have evidence of systematic bias? We can define a
new statistic that takes the absolute value of the difference between our
sample and the mean:

@examples[#:eval my-evaluator
          (define-statistic (abs-difference-from xs constant)
            (abs (- (mean xs) constant)))

          (define true-diff (abs-difference-from readings 105))]

To judge if the population is systematically biased, we'd need to know what
answer we'd get if we had many samples from the population, but we don't.
Instead we'll use the bootstrap: we'll repeatedly resample with replacement from
@racket[readings], taking samples the same size as the original dataset, and
compare each to the true value. Then we'll count what fraction of these
differences are larger than the difference we observed.

@examples[#:eval my-evaluator
          (define bootstrap-distribution
            (abs-difference-from
             (n-iid (empirical-dist readings) 12)
             105))

          (count (lambda (diff) (>= diff true-diff))
                 (sample bootstrap-distribution 100))]

So, of 100 samples from the bootstrap distribution, the majority were as far or
farther from 105 as our original sample. This suggests that 105 is a plausible
population mean---we have no evidence of bias.

Why is it interesting to do statistics this way? First, by writing our methods
as code, they are @emph{explicit}: there is no ambiguity in notation or in
phrasing. Second, our code @emph{invites exploration}: what would happen if I
used the median instead? What if the sample is much larger? What if the true
value were 110 or 264? What if we used the raw difference instead of the
absolute difference, to get a one-tailed test? And so on and so on. Instead of
learning from rote, we can learn by experimentation, and get the results
intantly.

@section{Distributions}

@defproc[(empirical-dist [data (Listof Real)])
         Empirical-Dist]{
 Creates an empirical distribution from the list of provided
 data points. The empirical distribution supports @racket[sample], @racket[cdf],
 @racket[inv-cdf], @racket[pdf], like other common distributions. Samples from
 an empirical distribution are bootstrap samples.

 @examples[#:eval my-evaluator

           (define foo (empirical-dist (list 1 2 2 3 4 5)))
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

           (pdf three-normals #(0 0 0))]
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
           (adder 1 2)
           (sample (adder 1 (normal-dist)))]

 If called with constant arguments, as in @racket[(adder 1 2)] above, the test
 statistic returns a constant. If any of the arguments are random variables, it
 returns a distribution by sampling from the distributions of its arguments.
}
