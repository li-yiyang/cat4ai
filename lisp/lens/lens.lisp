;;; lens.lisp --- Lens class

;; File:        lens.lisp
;; Description: Lens class
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 01:48
;; Version: 0.0.0
;; Last-Updated: 2024-12-07 02:50
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

(in-package :cat4ai.lens)

(defclass lens (abstract-morphism)
  ((forward
    :initarg :forward
    :initform (error "Missing forward")
    :documentation
    "forward: f : A -> B")
   (backward
    :initarg :backward
    :initform (error "Missing forward")
    :documentation
    "backward: f* : A x B' -> A'")
   (backward-arity
    :initform 2
    :initarg  :backward-arity
    :documentation
    "arity number for lens backward. ")
   (backward-dest-arity
    :initform 2
    :initarg  :backward-dest-arity
    :documentation
    "dest arity number for lens backward. "))
  (:default-initargs :arity               1
                     :dest-arity          1
                     :backward-arity      2
                     :backward-dest-arity 1)
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
  (declare (morphism forward backward))
  (assert (~< (arity forward) (arity backward)))
  (make-instance 'lens :forward             forward
                       :backward            backward
                       :arity               (arity      forward)
                       :dest-arity          (dest-arity forward)
                       :backward-arity      (arity      backward)
                       :backward-dest-arity (dest-arity backward)))

(defmethod composable ((lens1 lens) (lens2 lens))
  (let-slot* ((lens1 (f1 arity) (b1 backward-arity) (fd1 dest-arity))
              (lens2 (f2 arity) (b2 backward-arity) (bd2 backward-dest-arity)))
    (and (= fd1 f2) (= b1 (+ bd2 f1)) (< fd1 b2))))

(defmethod compose ((lens1 lens) (lens2 lens))
  (let-slot* ((lens1 (fwd1     forward)
                     (bwd1     backward)
                     (arity1   arity)
                     (darity1  dest-arity))
              (lens2 (fwd2     forward)
                     (bwd2     backward)
                     (barity2  backward-arity)
                     (bdarity2 backward-dest-arity)))
    (let* ((fwd  (compose fwd1 fwd2))
           ;; the `bwd' is like:
           ;;
           ;;   -- arity1 -----+--> [fwd1] -- darity1 ----+
           ;;                  |                          |
           ;;      ########<---+    ########<- darity1 ---+
           ;;   <--# bwd1 #<--------# bwd2 #  (barity2)
           ;;      ########         ########<------------------
           (args (dummy-args-list (+ arity1 (- barity2 darity1))))
           (farg (subseq args 0 arity1))
           (garg (last   args (- barity2 darity1)))
           (body `(funcall ,fwd1 ,@farg))
           (body (if (= 1 darity1)
                     `(-> ,bwd2 ,body ,@garg)
                     `(-<> ,bwd2 (nconc (multiple-value-list ,body)
                                          (list ,@garg)))))
           (body (if (= 1 bdarity2)
                     `(-> ,bwd1 ,@farg ,body)
                     `(-<> ,bwd1 (nconc (multiple-value-list ,body)
                                        (list ,@garg)))))
           (bwd  (eval (print `(lambda ,args ,body)))))
      (lens fwd bwd))))

(defmethod -> ((lens lens) &rest args)
  (let-slot* ((lens forward))
    (apply #'-> forward args)))

(defgeneric <- (lens &rest args)
  (:documentation
   "Backward map of `lens'. "))

(defgeneric <>- (lens args)
  (:documentation
   "Backward apply of `lens'. "))

(defmethod <- ((lens lens) &rest args)
  (let-slot* ((lens backward))
    (apply backward args)))

(defmethod <>- ((lens lens) args)
  (let-slot* ((lens backward))
    (apply backward args)))

;;; lens.lisp ends here
