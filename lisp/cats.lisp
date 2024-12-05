;;; cats.lisp --- Basic Category interfaces

;; File:        cats.lisp
;; Description: Basic Category interfaces
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 01:55
;; Version: 0.0.0
;; Last-Updated: 2024-12-05 01:55
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

(in-package :cat4ai)

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

See `composes' for compose multiple morphisms. "))

(defmethod compose ((f function) (g function))
  (lambda (&rest args) (multiple-value-call g (apply f args))))

(defgeneric combine (h f &rest more-fns)
  (:documentation
   "Combine output of `f' and `g' with `h'.
Often denoted as <f, g>;h : AxB -> h(f(AxB), g(AxB)).

        ########################
        # +-- f        ---+    #
   args # |               |    #
  =======+-- more-fns ---+== h -----
        # |               |    #
        # +-- ...      ---+    #
        ########################

See `compose' for chained function call. "))

(defmethod combine ((h function) (f function) &rest more-fns)
  (assert (every #'functionp more-fns))
  (let ((fns (cons f more-fns)))
    (lambda (&rest args)
      (apply h (mapcar* (fns) (apply fns args))))))

(defgeneric -> (morphism &rest args)
  (:documentation
   "Map `args' by `morphism'.

Often denoted as:
+ fn: arg -> (-> fn arg)
+ fn: arg x arg -> (-> fn arg arg)
  (you may say it as Cartesian production) "))

(defmethod -> ((fn function) &rest args)
  (apply fn args))

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

(defun pass (&optional (nth 0) &rest more-nths)
  "Pass only nth input value as return values.

        ########
        # +--X #
   args # |    # (nth)
  ========+-------------
        # |    # (more-nths)
        # +-------------
        ########

Example:

    (composes #'floor (pass 0) #'1+)

See `lcurry' and `rcurry' for unpassing the arguments.
"
  (lambda (&rest args)
    (values-list
     (mapcar (lambda (i) (nth i args))
             (cons nth more-nths)))))

(defun lcurry (fn &rest curried-args)
  "Make a curry.

 args ###########
==========+     #
      #   |- fn -----
      # ==+ (curried-args)
      ###########

See `rcurry' for argument on right."
  (lambda (&rest args)
    (apply fn (append curried-args args))))

(defun rcurry (fn &rest curried-args)
  "Make a curry function.

       ###########
       # ==+     #
  args #   |- fn -----
 =========+ (curried-args)
       ###########

See `lcurry' for argument on left."
  (lambda (&rest args)
    (apply fn (append args curried-args))))

;;; cats.lisp ends here
