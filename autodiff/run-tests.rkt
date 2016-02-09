#lang racket/base

#|  run-tests.ss: Run all tests of deriv package: "mzscheme -u run-tests.ss"
    Copyright (C) 2007 Will M. Farr <farr@mit.edu>

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


(require "deriv-lang-test.rkt"
	 rackunit
	 rackunit/text-ui)

(define all-tests
  (test-suite
   "all tests"
   deriv-lang-test-suite))

(exit (run-tests all-tests 'verbose))
