;; NS-NAMESPACE
;;
;; Copyright (c) 2012, Chris Barrett
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;
;; DESCRIPTION:
;; Contains NAMESPACE macro, which is the public interface for definining
;; namespaces and importing/exporting namespaced symbols.
;;

(provide 'ns-namespace)
(require 'ns-core)
(require 'ns-operators)
(require 'ns-definitions)
(eval-when-compile (require 'cl))
(setq lexical-binding t)

;;; -------------------------- Auxiliary Functions -----------------------------

(defun* ns/delete-nth (nth xs &key (count 1))
  (delete-if (lambda (x) t) xs :start nth :count count))

(defun ns/delete-keyword-and-arg (key xs)
  (let ((pos (position key xs)))
    (if pos
        (ns/delete-nth pos xs :count 2)
      xs)))

(defun ns/destructure-dep (handler)
  "Calls HANDLER to load the dependency form, provided the :when and :unless keywords do not override this.."
  (declare (indent 1))
  `(lambda (dep)
     (destructuring-bind
         (sym &rest autos &key (when t) (unless nil) &allow-other-keys)
         (if (sequencep dep) dep (list dep))
       (let* (
              (w     (if (symbolp when) when (eval when)))
              (u     (if (symbolp unless) unless (eval unless)))
              (autos (ns/delete-keyword-and-arg :when autos))
              (autos (ns/delete-keyword-and-arg :unless autos))
              )
         (when (and w (not u))
           (funcall ,handler sym autos))))))


(defun ns/handle-import (from-ns into-ns deps)
  "Load dependencies. If dependenices is empty, load all symbols exported by FROM-NS."
  (if deps
      (loop for sym in deps do (ns/import from-ns into-ns sym))
    (ns/import-all from-ns into-ns)))


(defun ns/autoload-dep (feature dep)
  "Autoload symbols from FEATURE, where DEP is a list of symbols to autoload."
  (let ((feat (or (when (stringp feature) feature)
                  (symbol-file feature) (symbol-name feature))))
    (if (symbolp dep)
        (autoload dep feat)
      (destructuring-bind (symbol &key docstring interactive type
                                  &allow-other-keys)
          dep
        (autoload symbol feat docstring interactive type)))))


(defun ns/join-dirs (&rest directories)
  (reduce (lambda (l r) (file-name-as-directory (concat l r)))
          directories))

(defun ns/ns->file (base ns)
  "Return a relative filepath for a given namespace."
  (let* ((xs   (split-string (symbol-name ns) "[.]"))
         (path (apply #'ns/join-dirs base xs))
         (path (substring path 0 -1)))
    (when xs
      (concat path ".el"))))


(defun ns/handle-use (base feature autoloads)
  "Autoload the specified symbols from namespace or feature FEATURE."
  (let* ((path (ns/ns->file base feature))
         (file (when (file-exists-p path) path)))
    (cond
     (autoloads
      (loop for dep in autoloads collect (ns/autoload-dep (or file feature) dep)))
     (file
      (load file))
     (t
      (require feature)))))


(defun ns/handle-pkg (pkg autoloads)
  "Require a package from an online repository, downloading it if needed."
  (unless (package-installed-p pkg)
    (package-install pkg))
  (if autoloads
      (loop for dep in autoloads collect (ns/autoload-dep pkg dep))
    (require pkg)))


;;; -------------------------- Namespace Macro ---------------------------------

(defmacro* namespace (name &key import export use packages)
  "Define or reopen an existing namespace.
The keyword agruments allow you to define exports, imports and dependencies on Emacs features and online packages.

NAME should be a symbol used to identify the namespace and qualify its members.

OPTIONAL KEYWORD ARGUMENTS
--------------------------

IMPORT is a vector of the form [ dep ... ], where each `dep` is either:
  1. a namespace whose members will be all be imported
  2. a list of the form (ns & syms ... ), where `ns` is a namespace identifier and each sym is a member to import.

EXPORT is a vector of the form [ sym ... ], where each `sym` is a symbol to make publicly accessible.

PACKAGES is a vector of the form [ dep ... ], where each `dep` is either:
  1. a package to be downloaded and loaded
  2. a list of the form (pkg & load ... ), where `pkg` is the package name and each `load` is a LOAD FORM as described later.

USE is a vector of the form [ dep ... ], where each `dep` is either:
  1. an elisp file or namespace to load, where periods are taken as path delimeters
  2. an emacs feature to require
  3. a list of the form (feature & load ... ) where `feature` is an Emacs feature and each `load` is a LOAD FORM, described below.

LOAD FORMS
----------
A LOAD FORM represents an item that will be autoloaded. It is either
  1. a symbol
  2. a list of the form (sym &key interactive type docstring) that will be passed to the Emacs autoload function. See the documentation for AUTOLOAD for an explanation of these symbols.
"
  (declare (indent 1))
  (assert (symbolp name))
  (assert (not (string-match-p "/" (symbol-name name))) ()
          "Invalid namespace identifier: `%s`
Forward-slashes (`/`) cannot be used." name)
  (provide name)
  (@using ns
    ;; Export the given symbols.
    (mapc (lambda (x) (ns/export name x))
          export)
    ;; Import the given symbols from other namespaces.
    (mapc (ns/destructure-dep (lambda (x xs) (ns/handle-import x name xs)))
          import)
    ;; download and load packages.
    (mapc (ns/destructure-dep (lambda (x xs) (ns/handle-pkg x xs)))
          packages)
    ;; Load emacs features and files.
    (mapc (ns/destructure-dep (lambda (x xs) (ns/handle-use *ns-base-path* x xs)))
          use))
  ;; Rebind the current namespace.
  (setq ns/current-ns name)
  `',name)


;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
