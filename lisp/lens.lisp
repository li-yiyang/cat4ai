;;; lens.lisp --- Lens class

;; File:        lens.lisp
;; Description: Lens class
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 01:48
;; Version: 0.0.0
;; Last-Updated: 2024-12-05 01:48
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

(defclass lens ()
  ((forward
    :initarg :forward
    :initform (error "Missing forward")
    :documentation
    "forward: f : A -> B")
   (backward
    :initarg :backward
    :initform (error "Missing forward")
    :documentation
    "backward: f* : A x B' -> A'"))
  (:documentation
   "The morphisms of Lens(C).

To make a lens, use `lens' function:

    (lens (lambda (a)    ...)
          (lambda (a b*) ...))

Definition:
The category of Lenses for any Cartesian category C consists of:
+ objects: pairs (A, A')
  A, A' in C;
+ morphism: (f, f*): (A, A') -> (B, B')
  + `forward':  f  : A -> B
  + `backward': f* : A x B' -> A'
+ morphism compose:  (f, f*);(g, g*) = (f;g, <𝜋0, <𝜋0;f, 𝜋1>;g*>;f*);
+ identity morphism: (A, A') is the pair (1_A, 𝜋1)

Graph:

      #####################
      #                   #
      #  +---> f -----------> B
   A  #  |                #
   ------+---------+      #
      #            |      #
      # ######     |      #
   A' # #    # <---+      #
   <--- # f* # <-------------- B'
      # ######            #
      #####################
"))

(defun lens (forward backward)
  "Make a morphism in Lens(C). "
  (declare (function forward backward))
  (make-instance 'lens :forward forward :backward backward))

(defmethod compose ((lens1 lens) (lens2 lens))
  (let-slot* ((lens1 (fwd1 forward) (bwd1 backward))
              (lens2 (fwd2 forward) (bwd2 backward)))
    (lens (compose fwd1 fwd2)
          (combine bwd1
                   (pass 0)
                   (combine bwd2 (compose (pass 0) fwd1) (pass 1))))))

(defmethod -> ((lens lens) &rest args)
  (let-slot* ((lens forward))
    (-> (compose (pass) forward) args)))

(defgeneric <- (lens &rest args)
  (:documentation
   "Backward map of `lens'. "))

(defmethod <- ((lens lens) &rest args)
  (let-slot* ((lens backward))
    (apply backward args)))

;;; lens.lisp ends here
