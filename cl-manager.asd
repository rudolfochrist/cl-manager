;;; cl-manager.asd

;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defsystem "cl-manager"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/cl-manager"
  :bug-tracker "https://github.com/rudolfochrist/cl-manager/issues"
  :source-control (:git "https://github.com/rudolfochrist/cl-manager.git")
  :version (:read-file-line "version")
  :depends-on ((:require "asdf")
               (:require "uiop"))
  :components ((:file "manager"))
  :description "A Common Lisp Package/System/Project/Library Manager"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt"))
  :perform (load-op :before (c o)
                    (when (= 0 (length (uiop:run-program "command -v git"
                                                         :output '(:string :stripped t)
                                                         :ignore-error-status t)))
                      (error "Please install `git'. Refer to https://git-scm.com/ for details."))
                    (when (= 0 (length (uiop:run-program "command -v curl"
                                                         :output '(:string :stripped t)
                                                         :ignore-error-status t)))
                      (error "Please install `curl'. Refer to your system's package manager.")))
  :perform (load-op :after (c o)
                    (let ((systems (asdf:system-relative-pathname "cl-manager" "systems.txt")))
                      (if (probe-file systems)
                          (uiop:symbol-call :cl-manager :make-index-table systems)
                          (uiop:symbol-call :cl-manager :update-index)))
                    (uiop:symbol-call :cl-manager :register-search-functions)))

