;;; cat4ai.asd --- ASDF system definition for Categories for AI

;; File:        cat4ai.asd
;; Description: ASDF system definition for Categories for AI
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-12-05 00:52
;; Version: 0.0.0
;; Last-Updated: 2024-12-05 00:52
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

(asdf:defsystem #:cat4ai
  :author ("凉凉")
  :version "0"
  :description
  "CL Implementation for Categories for AI.
Note that this is mainly for learning purpose and
is not expected to be a serious ML library. "
  :depends-on (ryo alexandria)
  :serial t
  :components
  ((:module   lisp
    :pathname "lisp"
    :components
    ((:file "package")

     (:module   utils
      :pathname "utils"
      :components
      ((:file "fns")
       (:file "macros")))

     (:module   cats
      :pathname "cats"
      :components
      ((:file "cats")
       (:file "cats-for-fn")
       (:file "morphism")))

     (:module   lens
      :pathname "lens"
      :components
      ((:file   "lens")))
     ))))


;;; cat4ai.asd ends here
