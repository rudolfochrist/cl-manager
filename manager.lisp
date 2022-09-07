;;; manager.lisp

;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:cl-manager
  (:nicknames #:clm)
  (:use :cl)
  (:export
   #:env
   #:install
   #:update-index
   #:current-directory-search
   #:dot-clm-directory-search
   #:register-search-functions
   #:load-system
   #:update
   #:find-system))

(in-package #:cl-manager)


(defvar *env* (uiop:getcwd)
  "The cl-manager working directory")


(define-condition clm-error (error)
  ((message :accessor clm-errro-message
            :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~A" (clm-errro-message condition)))))


(define-condition no-env-error (clm-error)
  ())


(defun env ()
  "Path to the CLM environment."
  (if (null *env*)
      (restart-case
          (error 'no-env-error
                 :message "Environment not initialized.")
        (use-cwd ()
          :report (lambda (stream)
                    (format stream "Set dep-name directory (~A) as environment."
                            (uiop:getcwd)))
          (setf (env) (uiop:getcwd))))
      *env*))


(defun (setf env) (path)
  "Set the CLM environment to PATH."
  (setf *env* path))


(defun clm-install-directory ()
  (merge-pathnames ".clm/" (env)))


(defun empty-string-p (string)
  (= 0 (length string)))


(defun exec (command)
  (uiop:run-program command
                    :output '(:string :stripped t)
                    :error-output :output))


(defun curl-file (url filename)
  (exec (format nil "curl -fsSL ~A -o ~A" url filename)))


(defun qprint (message &optional stream &rest arguments)
  (if (null arguments)
      (format (or stream t) "~&; ~A~%" message)
      (apply #'format
             stream
             (format nil "~&; ~A~%" message)
             arguments)))


(defun hash-keys (hash-table)
  (loop for k being the hash-keys of hash-table
        collect k))


(defun hash-values (hash-table)
  (loop for v being the hash-values of hash-table
        collect v))


(defstruct dep
  project system-name source ref)


(defun parse-clmfile (clmfile)
  (with-open-file (f clmfile)
    (loop with deps = (make-hash-table :test 'equal)
          for line = (read-line f nil nil)
          while line
          when (and (not (empty-string-p line))
                    (not (uiop:string-prefix-p "#" line)))
            do (destructuring-bind (system-name &optional ref)
                   (uiop:split-string line)
                 (setf (gethash system-name deps)
                       (make-dep :system-name system-name
                                 :ref ref)))
          finally (return deps))))

(defvar *index-url* "https://raw.githubusercontent.com/rudolfochrist/clm-projects/master/systems.txt")
(defvar *index* nil)

(defstruct %system
  project system-name source dependencies)


(define-condition system-not-found (clm-error)
  ((system :accessor missing-system
           :initarg :system))
  (:report (lambda (condition stream)
             (format stream "System ~A not found!" (missing-system condition)))))


(defun find-system (name)
  "Lookup system with NAME in index."
  (multiple-value-bind (system foundp)
      (gethash name *index*)
    (unless foundp
      (error 'system-not-found :system name))
    system))


(defun make-index-table (systems-file)
  (with-open-file (f systems-file)
    (read-line f)                       ; Skip comment.
    (loop with index = (make-hash-table :test 'equal)
          for line = (read-line f nil nil)
          while line
          unless (empty-string-p line)
            do (destructuring-bind (project system-name source &rest dependencies)
                   (uiop:split-string line)
                 (setf (gethash system-name index)
                       (make-%system :project project
                                     :system-name system-name
                                     :source source
                                     :dependencies (remove-if #'empty-string-p dependencies))))
          finally (setf *index* index)))
  *index*)


(defun update-index (&optional (url *index-url*))
  (qprint "Updating index...")
  (let ((systems-file (asdf:system-relative-pathname "cl-manager" "systems.txt")))
    (curl-file url systems-file)
    (make-index-table systems-file)))


(defvar *system-blacklist*
  (list "asdf" "uiop"))


(defun blacklistp (name)
  (member name *system-blacklist* :test #'string=))

(defun resolve-dependencies (clmfile)
  (loop with deps = (parse-clmfile clmfile)
        with q = (hash-keys deps)
        while q
        do (let ((dep-name (pop q)))
             (unless (blacklistp dep-name)
               (multiple-value-bind (dep foundp)
                   (gethash dep-name deps)
                 (let ((%system (find-system dep-name)))
                   (if foundp
                       (setf (dep-project dep)
                             (%system-project %system)
                             (dep-source dep)
                             (%system-source %system))
                       (setf (gethash dep-name deps)
                             (make-dep :project (%system-project %system)
                                       :system-name dep-name
                                       :source (%system-source %system))))
                   (setf q (append q (%system-dependencies %system)))))))
        finally (return (unique-project-dependencies deps))))


(defun unique-project-dependencies (dependency-table)
  (loop for dep being the hash-values of dependency-table
        collect dep into deps
        finally (return (remove-duplicates deps :test #'string= :key #'dep-project))))


(defun write-lockfile (dependencies)
  (with-open-file (out (merge-pathnames "clm.lock" (env))
                       :direction :output
                       :if-exists :supersede)
    (loop for dep in dependencies
          do (format out "~&~A ~A~@[ ~A~]~%"
                     (dep-project dep)
                     (dep-source dep)
                     (dep-ref dep)))))


(defun read-lockfile (lockfile)
  (with-open-file (in lockfile)
    (loop for line = (read-line in nil nil)
          while line
          collect (destructuring-bind (project source &optional ref)
                      (uiop:split-string line)
                    (make-dep :project project
                              :source source
                              :ref ref)))))



(defun download-dependencies (&optional wipe)
  (when wipe
    (uiop:delete-directory-tree (merge-pathnames ".clm/" (env)) :validate t))
  (let ((deps (read-lockfile (merge-pathnames "clm.lock" (env)))))
    (loop for dep in deps
          do (qprint "Installing ~A" t (dep-project dep))
          unless (probe-file (merge-pathnames (format nil ".clm/~A" (dep-project dep)) (env)))
            do (exec
                (format nil "git clone --depth 1~@[ -b ~A~] ~A .clm/~A"
                        (dep-ref dep)
                        (dep-source dep)
                        (dep-project dep)))
          finally (return (values)))))


(define-condition uninitialized-git (clm-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Not a git repository: ~A~%Please run `git init'." (env)))))


(defun install (&optional force)
  "Install systems defined in CLMFILE."
  (when (or force
            (not (probe-file (merge-pathnames "clm.lock" (env)))))
    (write-lockfile (resolve-dependencies (merge-pathnames "clmfile" (env)))))
  (download-dependencies force))


(defun update ()
  "Updated dependencies."
  ;; TODO: Future improvement: More intelligent update process.
  (install t))


(defun load-system (name &key verbose silent)
  "Load system with NAME.

If VERBOSE is non-nil display verbose output."
  (let ((*load-verbose* nil)
        (*compile-verbose* nil)
        (*load-print* nil)
        (*compile-print* nil))
    (unless silent
      (qprint "Loading ~A with dependencies." *standard-output* name))
    (handler-bind ((warning #'muffle-warning)
                   #+sbcl (sb-ext:compiler-note #'muffle-warning))
      (restart-case
          (asdf:load-system name :verbose verbose)
        (install ()
          :report "Unmet dependencies. Install them?"
          :test (lambda (condition) (typep condition 'asdf/find-component:missing-dependency))
          (install)
          (load-system name :verbose verbose :silent t))))))



(defun current-directory-search (name)
  "Search the current directory for system NAME."
  (probe-file (make-pathname :defaults (uiop:getcwd)
                             :name (asdf:primary-system-name name)
                             :type "asd")))


(defun dot-clm-directory-search (name)
  "Search .clm directory for system NAME."
  (handler-case
      (find-if (lambda (p)
                 (and (string= (asdf:primary-system-name name) (pathname-name p))
                      (string= "asd" (pathname-type p))))
               (uiop:directory-files (merge-pathnames ".clm/" (env)) "**/*.asd"))
    (no-env-error (condition)
      (declare (ignore condition)))))


(defun register-search-functions ()
  (setf asdf:*system-definition-search-functions*
        (append (list 'current-directory-search 'dot-clm-directory-search)
                asdf:*system-definition-search-functions*)))
