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

(defclass para ()
  ((morphism
    :initarg  :morphism
    :initform (error "Missing `:morphism'. ")
    :documentation
    "A pair map: f : P ⊗ A -> B"))
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
+ compose: (P, f) : A -> B ○ (P', f') : B -> C is
  (P' ⊗ P, (1 ⊗ f);g)

Graph:

             | P                | Q
             V                  V
           #####              #####
    A ---> # f # ---> B  ---> # g # ---> C
           #####              #####
"))

(defun para (morphism)
  "Define a Parametrized Morphism. "
  (make-instance 'para :morphism morphism))

(defgeneric repara (para map)
  (:documentation
   "
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

(defmethod repara ((para para) (map function))
  (let-slot* ((para morphism))
    (setf morphism
          (lambda (param arg)
            (funcall morphism (funcall map param) arg)))))

(defmethod compose ((para1 para) (para2 para))
  (let-slot* ((para1 (f morphism))
              (para2 (g morphism)))
    (para (lambda (param &rest param-arg)            ; Q . P arg
            (funcall g (apply f param-arg))))))

(defmethod -> ((para para) &rest args)
  (let-slot* ((para morphism))
    (apply morphism args)))

;;; para.lisp ends here
