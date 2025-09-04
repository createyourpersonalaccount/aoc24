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

(define-module (utils)
  #:export (when-0
            search-all-patterns))

(use-modules (ice-9 textual-ports)
             (ice-9 peg)
             (srfi srfi-1)
             (srfi srfi-41)
             (srfi srfi-171))

(define-public (file-contents filename)
  "The file contents of the text file."
  (call-with-input-file filename get-string-all))

(define-public (remove-empty strings)
  "Remove empty strings."
  (remove string-null? strings))

(define-public (sum xs)
  "Sum the list of numbers."
  (reduce + 0 xs))

(define (hashq-1+ h n)
  (hashq-set! h n (1+ (hashq-ref h n 0))))

(define-public (counter xs)
  "A hash table with key:count."
  (let ((h (make-hash-table)))
    (map (cute hashq-1+ h <>) xs)
    h))

;;; This is a variant of `when' that returns 0 if the predicate is
;;; false.
(define-syntax when-0
  (syntax-rules ()
    ((when-0 pred expr expr* ...)
     (if pred
         (begin expr expr* ...)
         0))))

(define-stream (search-all-patterns nonterm-or-peg string)
  "A stream of all matches of the peg on the string."
  (let* ((pat (search-for-pattern nonterm-or-peg string))
         (end (or (peg:end pat) 0))
         (string (substring string end)))
    (if pat
        (stream-cons (peg:tree pat)
                     (search-all-patterns nonterm-or-peg string))
        stream-null)))
