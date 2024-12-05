;;; utils.lisp --- Utils functions and macros for cat4ai

;; File:        utils.lisp
;; Description: Utils functions and macros for cat4ai
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 10:42
;; Version: 0.0.0
;; Last-Updated: 2024-12-05 10:42
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

(defmacro let-slot* (bindings &body body)
  "Binds slot variable.

Example:

    (let-slot* ((obj1 (slot11 slot1) (slot12 slot2))
                (obj2 (slot21 slot1) (slot22 slot2)))
      ,@body)
"

  (if (endp bindings)
      `(progn ,@body)
      (let* ((binding (first bindings))
             (obj     (first binding))
             (slots   (rest  binding)))
        `(with-slots ,slots ,obj
           (let-slot* ,(rest bindings) ,@body)))))

;;; utils.lisp ends here
