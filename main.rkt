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

(require "distributions.rkt"
         "empirical-dist.rkt"
         "statistic.rkt"
         "basic-statistics.rkt"
         racket/list)

(provide (all-from-out "distributions.rkt"
                       "empirical-dist.rkt"
                       "statistic.rkt"
                       "basic-statistics.rkt")
         #%module-begin #%app #%datum #%top #%top-interaction
         lambda if cond define require parameterize
         list vector count filter map
         first rest second third fourth fifth sixth seventh eighth ninth tenth
         last
         for for/list)
