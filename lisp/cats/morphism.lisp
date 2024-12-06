;;; morphism.lisp --- Type and bas abstract class for morphism

;; File:        morphism.lisp
;; Description: Type and bas abstract class for morphism
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-06 14:54
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

(defclass abstract-morphism ()
  ((arity      :initform 1
               :initarg  :arity)
   (dest-arity :initform 1
               :initarg  :dest-arity))
  (:documentation
   "Base Abstract Class of Morphism. "))

(defmethod arity ((morphism abstract-morphism))
  (let-slot* ((morphism arity))
    (values arity arity)))

(defmethod dest-arity ((morphism abstract-morphism))
  (let-slot* ((morphism dest-arity))
    (values dest-arity dest-arity)))

(defun morphism-p (obj)
  "Test if `obj' is morphism. "
  (or (functionp obj) (typep obj 'abstract-morphism)))

(deftype morphism ()
  '(satisfies morphism-p))

;;; morphism.lisp ends here
