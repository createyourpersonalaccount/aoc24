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

(define-module (day-01))

(use-modules (ice-9 format)
             (srfi srfi-13)
             (srfi srfi-26)
             (utils))

(define input (file-contents "input/01"))

;;; A list of (x y) lists corresponding to the rows of the input file.
(define numbers
  (let ( ;; A list of strings, each is a line from the input file.
        (lines (remove-empty
                (string-split input #\newline))))
    ;; Parse each line into a list (x y).
    (map (compose (cute map string->number <>)
            string-tokenize)
         lines)))

(define list-1 (sort (map car numbers) <))

(define list-2 (sort (map cadr numbers) <))

(define (solution-1)
  (let ((diffs (map (compose abs -) list-1 list-2)))
    (sum diffs)))

(define (solution-2)
  (let* ((h (counter list-2))
         (scores (map (lambda (n) (* n (hashq-ref h n 0)))
                      list-1)))
    (sum scores)))
