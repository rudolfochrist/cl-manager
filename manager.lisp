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
   #:find-system
   #:add-local-system
   #:install-system
   #:load-systems
   #:*system-blacklist*
   #:write-boot-file))

(in-package #:cl-manager)

;;; user options

(defvar *env* (uiop:getcwd)
  "The cl-manager working directory")


(defvar *system-blacklist* (list "asdf" "uiop")
  "List of blacklisted systems.

Each member won't be installed when it is found between the
dependencies of a system. This possibly can break some stuff. So I
guess you know what you're doing.")

;;; variables

(defvar *index-url* "https://raw.githubusercontent.com/rudolfochrist/clm-projects/master/systems.txt")
(defvar *local-index-file* (merge-pathnames "clm/local.txt" (uiop:xdg-data-home)))
(defvar *index* nil)

;;; conditions

(define-condition clm-error (error)
  ((message :accessor clm-errro-message
            :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~A" (clm-errro-message condition)))))


(define-condition no-env-error (clm-error)
  ())


(define-condition system-not-found (clm-error)
  ((system :accessor missing-system
           :initarg :system))
  (:report (lambda (condition stream)
             (format stream "System ~A not found!" (missing-system condition)))))


(define-condition uninitialized-git (clm-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Not a git repository: ~A~%Please run `git init'." (env)))))

;;; helpers

(defstruct %system
  project system-name source ref dependencies)


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


(defun alist-to-hash-table (alist)
  (loop with hash-table = (make-hash-table :test 'equal)
        for (key . value) in alist
        do (setf (gethash key hash-table) value)
        finally (return hash-table)))


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
                       (make-%system :system-name system-name
                                     :ref ref)))
          finally (return deps))))



(defun make-index-table (systems-file &key merge-index)
  (with-open-file (f systems-file)
    (loop with index = (if merge-index
                           *index*
                           (make-hash-table :test 'equal))
          for line = (read-line f nil nil)
          while line
          when (and (not (empty-string-p line))
                    (not (uiop:string-prefix-p "#" line)))
            do (destructuring-bind (project system-name source &rest dependencies)
                   (uiop:split-string line)
                 (setf (gethash system-name index)
                       (make-%system :project project
                                     :system-name system-name
                                     :source source
                                     :dependencies (remove-if #'empty-string-p dependencies))))
          finally (setf *index* index)))
  *index*)


(defun blacklistp (name)
  (member name *system-blacklist* :test #'string=))

(defun resolve-dependencies (deps)
  (loop with q = (hash-keys deps)
        while q
        do (let ((dep-name (pop q)))
             (unless (blacklistp dep-name)
               (multiple-value-bind (dep foundp)
                   (gethash dep-name deps)
                 (let ((%system (find-system dep-name)))
                   (if foundp
                       (setf (%system-project dep)
                             (%system-project %system)
                             (%system-source dep)
                             (%system-source %system))
                       (setf (gethash dep-name deps)
                             (make-%system :project (%system-project %system)
                                           :system-name dep-name
                                           :source (%system-source %system))))
                   (setf q (append q (%system-dependencies %system)))))))
        finally (return (unique-project-dependencies deps))))


(defun unique-project-dependencies (dependency-table)
  (loop for dep being the hash-values of dependency-table
        collect dep into deps
        finally (return (remove-duplicates deps :test #'string= :key #'%system-project))))


(defun write-lockfile (dependencies)
  (with-open-file (out (merge-pathnames "clm.lock" (env))
                       :direction :output
                       :if-exists :supersede)
    (loop for dep in dependencies
          do (format out "~&~A ~A~@[ ~A~]~%"
                     (%system-project dep)
                     (%system-source dep)
                     (%system-ref dep)))))


(defun read-lockfile (lockfile)
  (with-open-file (in lockfile)
    (loop for line = (read-line in nil nil)
          while line
          collect (destructuring-bind (project source &optional ref)
                      (uiop:split-string line)
                    (make-%system :project project
                                  :source source
                                  :ref ref)))))



(defun download-dependencies (deps)
  (loop for dep in deps
        do (qprint "Installing ~A" t (%system-project dep))
        unless (probe-file (merge-pathnames (format nil ".clm/~A" (%system-project dep)) (env)))
          do (exec
              (format nil "git clone --depth 1~@[ -b ~A~] ~A .clm/~A"
                      (%system-ref dep)
                      (%system-source dep)
                      (%system-project dep)))
        finally (return (values))))


;;; API

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



(defun find-system (name)
  "Lookup system with NAME in index."
  (multiple-value-bind (system foundp)
      (gethash name *index*)
    (unless foundp
      (error 'system-not-found :system name))
    system))


(defun update-index (&optional (url *index-url*))
  (qprint "Updating index...")
  (let ((systems-file (asdf:system-relative-pathname "cl-manager" "systems.txt")))
    (curl-file url systems-file)
    (make-index-table systems-file))
  ;; local index
  (dolist (index-file (uiop:directory-files (uiop:pathname-directory-pathname *local-index-file*)))
    (make-index-table index-file :merge-index t)))



(defun install (&optional force)
  "Install systems defined in CLMFILE."
  (when (or force
            (not (probe-file (merge-pathnames "clm.lock" (env)))))
    (write-lockfile (resolve-dependencies (parse-clmfile (merge-pathnames "clmfile" (env))))))
  (when force
    (uiop:delete-directory-tree (merge-pathnames ".clm/" (env)) :validate t))
  (download-dependencies (read-lockfile (merge-pathnames "clm.lock" (env)))))


(defun add-to-clmfile (name &optional ref)
  (with-open-file (stream (merge-pathnames "clmfile" (env))
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~&~A~@[ ~A~]~%" name ref)))


(defun install-system (name &key ref add)
  (download-dependencies
   (resolve-dependencies
    (alist-to-hash-table `((,name . ,(make-%system :system-name name :ref ref))))))
  (when add (add-to-clmfile name ref)))


(defun update ()
  "Updated dependencies."
  (delete-file (merge-pathnames "clm.lock" (env)))
  (install))


(defmethod load-system ((name string) &key verbose silent force ref add)
  "Load system with NAME.

If VERBOSE is non-nil display verbose output."
  (let ((*load-verbose* nil)
        (*compile-verbose* nil)
        (*load-print* nil)
        (*compile-print* nil))
    (unless silent
      (qprint "Loading ~A." *standard-output* name))
    (handler-bind (#+sbcl (sb-ext:compiler-note #'muffle-warning)
                   (warning #'muffle-warning))
      (handler-case
          (asdf:load-system name :verbose verbose :force force)
        (asdf/find-component:missing-component ()
          (install-system name :ref ref :add add)
          (load-system name :force force :silent t :verbose verbose))))))


(defmethod load-system ((name symbol) &key verbose silent force ref add)
  (load-system (string-downcase (string name))
               :verbose verbose
               :silent silent
               :force force
               :ref ref
               :add add))


(defun load-systems (systems &key verbose silent force)
  "Load a list of systems at once.

VERBOSE, SILENT, FORCE a passed as is to LOAD-SYSTEM."
  (mapc (lambda (system)
          (load-system system :verbose verbose :silent silent :force force))
        systems))


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
  (dolist (sf (list 'current-directory-search 'dot-clm-directory-search))
    (pushnew sf asdf:*system-definition-search-functions* :test #'eq))
  (setf asdf:*system-definition-search-functions*
        (nreverse asdf:*system-definition-search-functions*)))



(defun add-local-system (project system-name source &rest dependencies)
  "Add a system specification to the local index.

This can also be used to \"update\" systems with local informaton
e.g. using a different fork."
  (ensure-directories-exist *local-index-file*)
  (with-open-file (out *local-index-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~&~A ~A ~A~{ ~A~}~%"
            project system-name source dependencies)))


(defun write-boot-file (&optional (path (env)))
  (uiop:copy-file
   (asdf:system-relative-pathname "cl-manager" "templates/boot.lisp")
   (merge-pathnames "boot.lisp" path)))
