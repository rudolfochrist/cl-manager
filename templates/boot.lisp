;;; boot.lisp

;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; !!! WARNING !!!                                                         ;;;
;;;                                                                         ;;;
;;; Don't touch anything below unless you know what you're doing.           ;;;
;;;                                                                         ;;;
;;; Load this file to load dependencies installed with cl-manager without   ;;;
;;; loading cl-manager itself.                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'asdf)
#+sbcl (require 'sb-aclrepl)

(defun current-directory-search (name)
  "Search the current directory for system NAME."
  (probe-file (make-pathname :defaults (uiop:getcwd)
                             :name (asdf:primary-system-name name)
                             :type "asd")))


(defun dot-clm-directory-search (name)
  "Search .clm directory for system NAME."
  (find-if (lambda (p)
             (and (string= (asdf:primary-system-name name) (pathname-name p))
                  (string= "asd" (pathname-type p))))
           (uiop:directory-files (merge-pathnames ".clm/" (uiop:getcwd)) "**/*.asd")))


(setf asdf:*system-definition-search-functions*
      (nconc asdf:*system-definition-search-functions*
             (list 'current-directory-search 'dot-clm-directory-search)))
