;;; package.lisp --- Package definition for cat4ai

;; File:        package.lisp
;; Description: Package definition for cat4ai
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 00:56
;; Version: 0.0.0
;; Last-Updated: 2024-12-07 02:44
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

(defpackage #:cat4ai.utils
  (:use :cl :ryo.macros :ryo.fns)
  (:documentation
   "Utils functions and macros for CAT4AI. ")
  (:export
   #:~<
   #:~+
   #:dummy-args-list
   #:min-max-range-relationship
   #:let-slot*
   #:let-values*))

(defpackage #:cat4ai.cats
  (:use :cl :cat4ai.utils :ryo.macros :ryo.fns)
  (:documentation
   "Basic abstraction and experiments for category toolkits. ")
  (:export
   ;; cats
   #:arity
   #:dest-arity
   #:compose
   #:composes
   #:composable
   #:combine
   #:spread-combine
   #:->
   #:-<>

   ;; cats-for-fn
   ;; these functions should only be used as experiments
   #:pass
   #:pass-rest
   #:lcurry
   #:rcurry
   #:tunnel

   ;; morphism
   #:morphism
   #:morphism-p
   #:abstract-morphism
   ))

(defpackage #:cat4ai.lens
  (:use :cl :cat4ai.utils :cat4ai.cats :ryo.macros :ryo.fns)
  (:documentation
   "")
  (:export
   #:lens
   ))

;; (defpackage #:cat4ai
;;   (:use :cl :ryo.macros :ryo.fns)
;;   (:export
;;    ;; lens
;;    #:<-
;;    #:lens

;;    ;; para
;;    #:para))

;;; package.lisp ends here
