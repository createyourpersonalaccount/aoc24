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

(define-module (day-02))

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-13)
             (srfi srfi-26)
             (srfi srfi-171)
             (utils))

(define input (file-contents "input/02"))

;;; A list of number lists corresponding to the rows of the input file.
(define numbers
  (let ( ;; A list of strings, each is a line from the input file.
        (lines (remove-empty
                (string-split input #\newline))))
    ;; Parse each line into a list (x y).
    (map (compose (cute map string->number <>)
            string-tokenize)
         lines)))

(define (incompatible x y order)
  (or (eq? x y)
      (not (eq? (< x y) order))
      (> (abs (- x y)) 3)))

(define (safe-1 xs)
  (match xs
    ((x y . rest)
     (let ((order (< x y)))
       (define (safe-1-aux xs)
         (match xs
           ((x y . rest)
            (match-let (((x y . rest) xs))
              (if (incompatible x y order)
                  #f
                  (safe-1-aux (cons y rest)))))
           (_ #t)))
       (safe-1-aux xs)))
    (_ #f)))

(define (safe-2 xs)
  (match xs
    ((x y . rest)
     (let ((order (< x y)))
       (define (safe-2-aux xs n)
         (match xs
           ((x y . rest)
            (if (incompatible x y order)
                (if (= n 0)
                    #f
                    (or (safe-2-aux (cons x rest) (1- n))
                        (safe-2-aux (cons y rest) (1- n))))
                (safe-2-aux (cons y rest) n)))
           (_ #t)))
       (safe-2-aux xs 1)))
    (_ #f)))


;;; Map pred over lst and count how many are true.
(define (count-predicate pred lst)
  (list-transduce (tfilter pred)
                  rcount
                  numbers))

(define (solution-1)
  (count-predicate safe-1 numbers))

(define (solution-2)
  (count-predicate safe-2 numbers))

;;; An alternative solution where a port transducer is used. This
;;; means that the file is read line-by-line and then the "safety" of
;;; each line is computed and the results are tallied.

(define (line-to-numbers line)
  "Convert a file row into a list of numbers."
  (map string->number (string-tokenize line)))

(define (solution-1')
  (call-with-input-file "input/02"
    (lambda (port)
      (port-transduce (compose (tmap line-to-numbers)
                               (tfilter safe-1))
                      rcount
                      read-line
                      port))))
