;; Advent of Code 2024 solutions in GNU Guile.
;; Copyright (C) 2025  Nikolaos Chatzikonstantinou
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (day-03)
  #:export (find-muls
            find-muls-2
            solution-1
            solution-2))

(use-modules (ice-9 peg)
             (ice-9 match)
             (srfi srfi-41)
             (utils))

(define input (file-contents "input/03"))

(define-peg-string-patterns
  "digitP <- [0-9]
   mulS < 'mul'
   openP < '('
   commaP < ','
   closeP < ')'
   numberP <-- digitP digitP? digitP?
   doP <-- 'do()'
   dontP <-- 'don' ['] 't()'
   mulP <- mulS openP numberP commaP numberP closeP
   instrP <- doP / mulP / dontP")

(define score
  (match-lambda
    (((_ x) (_ y))
     (* (string->number x)
        (string->number y)))))

(define (score-patterns patterns)
  (stream-fold + 0
               (stream-map score patterns)))

(define (find-muls s)
  (let ((patterns (search-all-patterns mulP s)))
    (score-patterns patterns)))

(define (solution-1) (find-muls input))

(define (stream-filter-dont/do stream)
  "Filter all matches between a don't() do() pair."
  (let ((collect? #t))
    (stream-filter
     (match-lambda
       ;; Do() and don't() are not collected.
       (('doP   _) (set! collect? #t) #f)
       (('dontP _) (set! collect? #f) #f)
       ;; The rest are collected if the flag is set.
       (_ collect?))
     stream)))

(define (find-muls-2 s)
  (let* ((patterns (search-all-patterns instrP s)))
    (score-patterns (stream-filter-dont/do patterns))))

(define (solution-2) (find-muls-2 input))
