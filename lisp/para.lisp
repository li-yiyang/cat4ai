;;; para.lisp --- Parametrized Maps

;; File:        para.lisp
;; Description: Parametrized Maps
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 01:40
;; Version: 0.0.0
;; Last-Updated: 2024-12-05 01:40
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

(defclass para (abstract-morphism)
  ((morphism
    :initarg  :morphism
    :initform (error "Missing `:morphism'. ")
    :documentation
    "A pair map: f : P ⊗ A -> B"))
  (:default-initargs :arity      2
                     :dest-arity 1)
  (:documentation
   "Parametrized Morphism map of Para(C).

To define a parametrized morphism, use `para' function:

    (para (lambda (param arg) ...))

Definition:
A category Para(C) of a strict symmetric monoidal category C
(with monoidal product ⊗ and monoidal unit I):
+ object: C
+ morphism: f : P ⊗ A -> B is a pair (P, A) map (A, B ∈ C)
+ identity object: (I, 1_A) (I ⊗ A = A)
+ compose: compose of f1 : P1 ⊗ A -> B and f2 : P2 ⊗ A -> B
  is f : P2 ⊗ P1 ⊗ A -> B (and increase arity of para)

Graph:

             | P                | Q
             V                  V
           #####              #####
    A ---> # f # ---> B  ---> # g # ---> C
           #####              #####
"))

(defun para (morphism)
  "Define a Parametrized Morphism.
The `morphism' should be a map with arity number >= 2."
  (declare (morphism morphism))
  (assert (arity<= morphism 2))
  (if (typep morphism 'para)
      morphism
      (make-instance 'para :morphism morphism)))

(defgeneric repara (para map &rest more-maps)
  (:documentation
   "Reparametrize `para'.

The number of input maps should equal to (1- (arity para)).

Definition:
A reparametrisation of (P, f) : A -> B in Para(C)
by map α : Q -> P is the Para(X) map (Q, (α ⊗ 1_A);f) : A -> B.

Graph:

             | Q
             V
           #####
           # α #
           #####
             | P
             V
           #####
    A ---> # f # ---> B
           #####
"))

(defmethod repara ((para para) (map function) &rest more-maps)
  (assert (every #'morphism-p more-maps))
  (assert (length= more-maps (2- (arity para))))
  (let-slot* ((para morphism arity))
    (let* ((args   (dummy-args-list arity))
           (params (mapcar* ((f (cons map more-maps))
                             (p (subseq args 0 (1- (length args)))))
                     `(funcall ,f ,p))))
      (para (eval `(lambda ,args (funcall ,morphism ,@params ,@(last args))))))))

(defmethod composable ((para1 para) (para2 para))
  (< (dest-arity para1) (arity para2)))

(defmethod compose ((para1 para) (para2 para))
  (let-slot* ((para1 (f morphism) (f-dest dest-arity) (f-arg arity))
              (para2 (g morphism) (g-dest dest-arity) (g-arg arity)))
    (let* ((h-arg (1- (+ f-arg g-arg)))
           (args  (dummy-args-list h-arg))
           (fargs (subseq args (1- g-arg) (1- h-arg)))
           (gargs (subseq args 0          (1- g-arg)))
           (fcall `(funcall ,f ,@fargs ,@(last args)))
           (gcall (if (= 1 f-dest)
                      `(funcall ,g ,@gargs ,fcall)
                      `(apply ,g (nconc (list ,@gargs)
                                        (multiple-value-list ,fcall)))))
           (para  (para (eval `(lambda ,args ,gcall)))))
      (let-slot* ((para arity dest-arity))
        (setf arity      h-arg
              dest-arity g-dest)
        para))))

(defmethod -> ((para para) &rest args)
  (let-slot* ((para morphism))
    (apply morphism args)))

;;; para.lisp ends here
