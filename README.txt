1 NAME
======

  cl-manager --- A Common Lisp Package/System/Project/Library Manager


2 VERSION
=========

  ,----
  | 0.1
  `----


3 DISCLAIMER
============

  This is alpha software. Use at own risk!


4 SYNOPSIS
==========

  ,----
  | # clmfile
  | hunchentoot v1.3.1
  | cl-who
  | cl-ppcre 
  `----

  ,----
  | CL-USER> (clm:install)
  | ; Installing hunchentoot
  | ; Installing cl-who
  | ; Installing alexandria
  | ; Installing bordeaux-threads
  | ; Installing chunga
  | ; Installing cl+ssl
  | ; Installing cl-base64
  | ; Installing cl-fad
  | ; Installing cl-ppcre
  | ; Installing flexi-streams
  | ; Installing md5
  | ; Installing rfc2388
  | ; Installing trivial-backtrace
  | ; Installing usocket
  | ; Installing closer-mop
  | ; Installing global-vars
  | ; Installing postmodern
  | ; Installing split-sequence
  | ; Installing trivial-gray-streams
  | ; Installing cffi
  | ; Installing trivial-features
  | ; Installing trivial-garbage
  | ; Installing ironclad
  | ; Installing uax-15
  | ; Installing babel
  | ; No value
  | CL-USER> (clm:load-system "FOOBAR")
  | ; Loading FOOBAR with dependencies.
  | T
  `----


5 DESCRIPTION
=============

  `cl-manage' manages project dependencies with git.

  Goals:

  - Communication over HTTPS
  - Project-level dependencies (with pinning)
  - Minimal loading facilities (good for building executables)


6 PREREQUISITES
===============

  These programs have to be installed beforehand:

  - git
  - curl


7 INSTALLATION
==============

  Just put it somewhere ASDF finds it. I usually put it under
  `~/common-lisp' Optionally you can add

  ,----
  | (asdf:load-system "cl-manager" :verbose nil)
  `----

  in your init file to automatically load it.


8 `clmfile'
===========

  The `clmfile' specifies dependencies. Example:

  ,----
  | # This is a comment and is ignored
  | hunchentoot v1.3.1
  | cl-who
  | postmodern
  `----

  Each dependency can have an optional version specifier. This is a git
  branch/tag/commit hash. After the dependencies are installed a
  `clm.lock' file is written. You want both under version control.


9 FUNCTIONS & VARIABLES
=======================

  `env'
        The CLM environment. Most likely you want that to set to the
        current working directory. The environment determines where
        dependencies are installed to.

  `(setf env)'
        Set the environment.

  `load-system'
        Load a system by name. This works almost as the same as
        `asdf:load-system'.

  `install'
        Looks inside your `clmfile' and installs all dependency.

  `update'
        Updates installed dependencies.

  `update-index'
        Updates the index CLM refers to about new systems and the
        dependencies of dependencies. Etc.


10 AUTHOR
=========

  Sebastian Christ (<mailto:rudolfo.christ@pm.me>)


11 LICENSE
==========

  Released under the MPL-2.0 license.


12 SEE ALSO
===========

  - [Quicklisp]
  - [CLPM]
  - [borg]
  - [clm-projects]


[Quicklisp] <https://www.quicklisp.org/beta/>

[CLPM] <https://gitlab.common-lisp.net/clpm/clpm>

[borg] <https://github.com/emacscollective/borg>

[clm-projects] <https://github.com/rudolfochrist/clm-projects>
