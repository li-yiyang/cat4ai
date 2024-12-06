;;; cats-for-fn.lisp --- Category Toolkits for functions

;; File:        cats-for-fn.lisp
;; Description: Category Toolkits for functions
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-06 02:00
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

;; This file is the implementation of built-in function type
;; for the category morphism mapping maniuplation.
;;
;; The design idea is mainly inspired by SDF (Software Design for Flexibiliy)

(in-package :cat4ai.cats)

;; https://stackoverflow.com/questions/15465138/find-functions-arity-in-common-lisp
(declaim (inline arglist))
(defun arglist (fn)
  "Return the lambda-list of `fn'. "
  (declare (function fn))
  ;; I only use SBCL right now, so I removed all the
  ;; other implementation because I dont have environment
  ;; to test them all
  #+sbcl
  (sb-introspect:function-lambda-list fn)
  ;; This will only work for interpreted functions
  ;; so not recommanded to use unless no implementation
  ;; is supported
  #-(or sbcl)
  (second (function-lambda-expression fn)))

(defmethod arity ((fn function))
  (multiple-value-bind (required optional rest-p keyword allow-other-key-p)
      (alexandria:parse-ordinary-lambda-list (arglist fn))
    (let* ((min (length required))
           (opt (length optional))
           ;; (key (2* (length keyword)))
           (max (unless (or rest-p allow-other-key-p)
                  (+ ;; (* 2 key)
                     min opt))))
      (when keyword
        (warnf "arity of ~A ignores keywords ~{:~A~^, ~}" fn keyword))
      ;; The arity calculation only takes in count of &optional, &rest
      (values min max))))

(defmethod dest-arity ((fn function))
  ;; This is a trivial implementation
  (let ((values
          #+sbcl
          (car (last (sb-introspect:function-type fn)))
          #-(or sbcl)
          (car (last (function-lambda-expression fn)))))
    (cond ((null  values)
           (values 0 0))
          ;; for those could not extract the values list
          ;; return values as (1, infinity)
          ((or (not (listp values))
               (neq (first values) 'values))
           (values 1 nil))              ; at least return 1 value
          ;; dest-arity is calculated by counting the
          ;; ftype function values list, which should
          ;; be like (values ... &optional ...)
          (t (loop with min = 0
                   with opt = 0
                   with flag = nil
                   for sym in (rest values)
                   do (cond ((eq sym '&optional) (setf flag t))
                            (flag                (incf opt))
                            (t                   (incf min)))
                   finally (return (values min (+ opt min))))))))

;; to compose function, should keep the arity of `f' and dest-arity of `g'
;; as the same of the initial function.
(defmethod compose ((f function) (g function))
  (let* ((farglist (arglist f))
         (fcall    (multiple-value-bind (ordinary optional rest-p)
                       (alexandria:parse-ordinary-lambda-list farglist)
                     (if rest-p
                         `(apply ,f ,@ordinary ,@(mapcar #'first optional) ,rest-p)
                         `(funcall ,f ,@ordinary ,@(mapcar #'first optional))))))
    (eval `(lambda ,farglist (multiple-value-call ,g ,fcall)))))

(defmethod combine ((h function) (f function) &rest more-fns)
  (assert (every #'functionp more-fns))
  (let ((lambda-list (arglist f)))
    (multiple-value-bind (regular optional rest)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (let* ((opt-args  (mapcar #'first optional))
             (args-expr (if rest
                            `(nconc (list ,@regular ,@opt-args) ,rest)
                            `(list ,@regular ,@opt-args)))
             (args-fns  (mapcar* ((fn (cons f more-fns)))
                          `(multiple-value-list (apply ,fn args)))))
        (eval `(lambda ,lambda-list
                 (let ((args ,args-expr))
                   (apply ,h (nconc ,@args-fns)))))))))

(defmethod spread-combine ((h function) (f function) &rest more-fns)
  (assert (every #'functionp more-fns))
  (loop with i = 0 with arglist = ()
        for fn in (cons f more-fns)
        collect (loop for j below (arity fn)
                      for arg = (intern (fmt "ARG~D" i))
                      do (push arg arglist)
                      collect arg into f-list
                      do (incf i)
                      finally (return `(multiple-value-list
                                        (funcall ,fn ,@f-list))))
          into args-expr
        finally (return (eval `(lambda ,(nreverse arglist)
                                 (apply ,h (nconc ,@args-expr)))))))

(defmethod -> ((fn function) &rest args)
  (apply fn args))

(defmethod -<> ((fn function) args)
  (declare (list args))
  (apply fn args))

;;; Other helpful functions

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
  (eval `(lambda (&rest args)
           (values ,@(mapcar* ((i (cons nth more-nths))) `(nth ,i args))))))

(defun pass-rest (&optional (nth 1))
  "Pass rest input args (`nth' after input).
See `pass' for pass single arguments. "
  (lambda (&rest args) (values-list (nthcdr nth args))))

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

(defun tunnel (&optional (size 1))
  "Make a tunnel function take `size' arguments and return then as values. "
  (let ((args (dummy-args-list size)))
    (eval `(lambda ,args (values ,@args)))))

;;; cats-for-fn.lisp ends here
