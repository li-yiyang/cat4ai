;;; fns.lisp --- Utils functions for cat4ai

;; File:        fns.lisp
;; Description: Utils functions for cat4ai
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-06 22:00
;; Version: 0.0.0
;; Last-Updated: 2024-12-06 22:00
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/cat4ai
;; Keywords:
;; Compatibility:
;;
;;

;;; License
;;
;; this package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; this package is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package. If not, see <https://www.gnu.org/licenses/>.

(in-package :cat4ai.utils)

(declaim (inline ~< ~+))
(defun ~< (a b)
  "Test if `a' is less or equal to `b'.
If `a' or `b' is `nil', considering them as positive infinity. "
  (or (and (null a) (null b))
      (or  (null b) (and (not (null a))
                         (<= a b)))))

(defun ~+ (a b)
  "Add `a' and `b'.
If one of `a' or `b' is `nil', return `nil'. "
  (if (or (null a) (null b)) nil (+ a b)))

(defun dummy-args-list (min &optional (max min))
  "Generate a list of dummy args taking args from `min' to `max'.
Return values are:
1. ordinary symbol list
2. optional symbol list
3. rest symbol or `nil' if not"
  (cond ((null min)
         (values () () 'args))
        ((null max)
         (values (dotimes-collect (i min) (intern (fmt "ARG~D" i)))
                 ()
                 'args))
        (t
         (values (dotimes-collect (i min)         (intern (fmt "ARG~D" i)))
                 (dotimes-collect (i (- max min)) (intern (fmt "OPT~D" i)))
                 nil))))

;; TODO: need more edge test or proof
(declaim (inline min-max-range-relationshipe))
(defun min-max-range-relationship (min1 max1 min2 max2)
  "Return range relationship for:
+ `:overlap'     if ranges overlaps
+ `:equal'       if ranges are equal
+ `:non-overlap' if ranges are not overlaped
+ `:1-subset-2'  if range1 is subset (<) range2
+ `:2-subset-1'  if range2 is subset (<) range1

Note: if `min1' == `max2', will consider as `:non-overlap'. "
  (cond ((and (eq min1 min2) (eq max1 max2)) :equal)
        ((and (~< min2 min1) (~< max1 max2)) :1-subset-2)
        ((and (~< min1 min2) (~< max2 max1)) :2-subset-1)
        ((or  (~< max1 min2) (~< max2 min1)) :non-overlap)
        (t                                   :overlaps)))

;;; fns.lisp ends here
