;;; cats.lisp --- Basic Category interfaces

;; File:        cats.lisp
;; Description: Basic Category interfaces
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 01:55
;; Version: 0.0.0
;; Last-Updated: 2024-12-06 22:14
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

(in-package :cat4ai.cats)

;; Here the toolkit borrowed some ideas from SDF (Software Design
;; for Flexibility) to construct the function conbinator and the
;; other abstract morphism maps.

(defgeneric arity (morphism)
  (:documentation
   "Return min and max argument number as values.
If `morphism' support taking in infinite arguments,
the max value will be `nil'.

Definition:
Arity is the number of arguments or operands taken by `morphism'.

See `dest-arity' for function output arity. "))

(defgeneric dest-arity (morphism)
  (:documentation
   "Return min and max output values number as values.
If `morphism' output in infinite arguments,
the max value will be `nil'.

See `arity' for function input arity. "))

(defun arity-sum (fns-list &key (arity #'arity))
  "Sum up functions `fns-list' using `arity'.
Return values:
1. min of arity sum
2. max of arity sum

Para:
+ `arity': one of `arity' or `dest-arity'"
  (declare (list fns-list))
  (loop with min = 0 with max = 0
        for fn in fns-list
        do (let-values* (((fnmin fnmax) (funcall arity fn)))
             (setf min (~+ min fnmin)
                   max (~+ max fnmax)))
        finally (return (values min max))))

(defun arity-shape-eq (f g &key (f-arity #'dest-arity) (g-arity #'arity) (strict t))
  "Check if `f' and `g' arity is equal [ARG_f = ARG_g] (only when `strict'),
or f arity is subset of g arity [aka ARG_f < ARG_g] (unless `strict').
Return `t' if satisfies condition.

Para:
+ `f-arity' and `g-arity' should be `arity' or `dest-arity'
+ `strict' "
  (let-values* (((fmin fmax) (funcall f-arity f))
                ((gmin gmax) (funcall g-arity g)))
    (case (min-max-range-relationship fmin fmax gmin gmax)
      (:equal      t)
      (:1-subset-2 (not strict))
      (otherwise   nil))))

(defun arity>= (morphism min &key (max min) (arity #'arity) strict)
  "Test if `morphism' arity equal or supset of `min' and `max'.
Return `t' if satisfies. "
  (let-values* (((fmin fmax) (funcall arity morphism)))
    (case (min-max-range-relationship min max fmin fmax)
      (:equal      t)
      (:1-subset-2 (not strict))
      (otherwise   nil))))

(defun arity<= (morphism min &key (max nil) (arity #'arity) strict)
  "Test if `morphism' arity equal or subset of `min' and `max'.
Return `t' if satisfies. "
  (let-values* (((fmin fmax) (funcall arity morphism)))
    (case (min-max-range-relationship fmin fmax min max)
      (:equal      t)
      (:1-subset-2 (not strict))
      (otherwise   nil))))

(defgeneric compose (f g)
  (:documentation
   "Compose `f' and `g' in left-to-right sequence.
Often denoted as f;g, or g○f : x -> g(f(x)).

Develop:
+ to chain the values passing by the morphisms `f' and `g',
  the values are passed as values and &rest args.

  for example:
  + AxA -> AxBxC ; AxBxC -> D
    will be like compose function with lambda-list like (a b) and (a b c);
    and return values like (values a b c) for first morphism, (values d)
    for second morphism.
+ if two morphism `f' and `g' is composable, the arity should
  satisfies: dest-arity of `f' is equal or be subset of arity `g'

  see `compose' :before for the test

  for example:
  + (compose #'1+ #'floor)
    + f = `1+' dest-arity is (1, 1)
    + g = `floor' arity is (1, 2)
    + (1, 1) is subset of (1, 2) so it is okay to compose
  + but (compose #'floor #'1+) cannot

See `composes' for compose multiple morphisms. "))

(defgeneric composable (f g)
  (:documentation
   "Test if two morphism `f' and `g' can compose. "))

(defmethod composable (f g)
  (let-values* (((min-f max-f) (dest-arity f))
                ((min-g max-g) (arity      g)))
    (case (min-max-range-relationship min-f max-f min-g max-g)
      ((:equal :1-subset-2) t)
      (otherwise            nil))))

;; check if two morphism `f' and `g' is composable
;; by arity before doing compose
(defmethod compose :before (f g)
  (unless (composable f g)
    (errorf "~A and ~A is not in proper shape. " f g)))

(defgeneric combine (h f &rest more-fns)
  (:documentation
   "Combine output of `f' and `g' with `h'.
Often denoted as <f, g>;h : AxB -> h(f(AxB), g(AxB)).

        #########################
        # +-- f        ---+     #
   args # |               |     #
  ========+-- more-fns ---+== h -----
        # |               |     #
        # +-- ...      ---+     #
        #########################

Develop:
+ check before combine if functions can combine
  + the function input should be linted by `f':
    other functions' input arguments should be
    equal superset of f input arguments
  + the function output arguments summation should
    be equal or subset of h input arguments

See `compose' for chained function call. "))

;; check before combine if functions can combine
(defmethod combine :before (h f &rest more-fns)
  ;; the function input should be linted by `f'
  ;; other functions' input arguments should be
  ;; equal or superset of f
  (dolist (fn more-fns)
    (unless (arity-shape-eq f fn :f-arity #'arity
                                 :g-arity #'arity
                                 :strict  nil)
      (errorf "~A and ~A is not in proper shape. " f fn)))

  ;; the function output arguments summation should
  ;; be equal or subset of h input arguments
  (let-values* (((min  max)  (arity-sum (cons f more-fns)
                                        :arity #'dest-arity))
                ((hmin hmax) (arity h)))
    (case (min-max-range-relationship min max hmin hmax)
      ((:equal :1-subset-2) t)
      (otherwise (errorf "~{~A~^, ~} is not in proper shape with ~A. "
                         (cons f more-fns) h)))))

(defgeneric spread-combine (h f &rest more-fns)
  (:documentation
   "Split input args to `f' and `more-fns' and apply the results on `h'.

         ####################
         #   n              #
         # +--- f -------+  #
    args # | m           V  #
   --------+--- g -----> h------>
         # | (more-fns)  ^  #
         # +-------------+  #
         ####################

Develop:
+ check before spread-combine
  + functions should have no ambiguous input argument numbers,
    which means arity min and max should be equal
  + the output argument number of functions should
  "))

(defmethod spread-combine :before (h f &rest more-fns)
  ;; functions should have no ambiguous input arity
  (dolist (fn (cons f more-fns))
    (let-values* (((min max) (arity fn)))
      (unless (= min max)
        (warnf "~A has ambiguous arity, using arity min only" fn))))
  ;; output should combine
  (let-values* (((min  max)  (arity-sum (cons f more-fns)
                                        :arity #'dest-arity))
                ((hmin hmax) (arity h)))
    (case (min-max-range-relationship min max hmin hmax)
      ((:equal :1-subset-2) t)
      (otherwise (errorf "~{~A~^, ~} is not in proper shape with ~A. "
                         (cons f more-fns) h)))))

(defgeneric -> (morphism &rest args)
  (:documentation
   "Map `args' by `morphism'.

Often denoted as:
+ fn: arg -> (-> fn arg)
+ fn: arg x arg -> (-> fn arg arg)
  (you may say it as Cartesian production) "))

(defgeneric -<> (morphism args)
  (:documentation
   "Apply `morphism' on `args'. "))

(defun composes (fn &rest more-fns)
  "Compose a fn chains in left-to-right sequence.

    #################
    #               #
  ---- f - ... - g ----
    #               #
    #################

Example:

    (-> (composes #'1+ #'2*) i)

See `compose' for compose only two morphism.
"
  (if (endp more-fns)
      fn
      (apply #'composes
             (compose fn (first more-fns))
             (rest more-fns))))

;;; cats.lisp ends here
