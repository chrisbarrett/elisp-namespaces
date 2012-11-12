;;; namespaces.el --- An implementation of namespaces for Elisp, with an emphasis on immutabilty.
;;
;; Author: Chris Barrett
;; URL: https://github.com/chrisbarrett/elisp-namespaces
;; Version: 1.0.1
;;

;;; License:

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

;;; Commentary:
;;
;;   (require 'namespaces)
;;
;;   (namespace foo :export [hello])
;;   (defn hello () "Hello, world!")
;;
;;   (namespace bar)
;;   (foo/hello)    ; # => "Hello, world!"
;;
;;
;; See documentation at https://github.com/chrisbarrett/elisp-namespaces
;;

;;; Code:

(defcustom ns/base-path (concat user-emacs-directory "elisp/")
  "Defines the base directory for namespace resolution."
  :group 'namespaces)

;;; ================================ Core ======================================

(require 'cl)
(autoload 'package-installed-p "package")
(autoload 'package-install "package")

(defstruct ns-meta
  public?
  mutable?)

(defconst ns/symbols-table (make-hash-table :test 'equal)
  "hash * name -> ns-meta")


;;; --------------------------- Symbol Processing ---------------------------------

(defun ns/hash (sym)
  "Hash a symbol for insertion into lookup tables."
  (let ((hash (secure-hash 'sha256 (symbol-name sym))))
    (intern (concat "__ns/namespaced_sym__" hash))))

(defun ns/split-sym (sym)
  "Splits SYM into a tuple of (namespace * symbol)."
  (let* ((xs (split-string (symbol-name sym) "/"))
         (s1 (intern (car xs)))
         (s2 (when (nth 1 xs) (intern (nth 1 xs)))))
    (if (< 1 (length xs))
        `(,s1 . ,s2)
      `(,nil . ,s1))))

(defun ns/qualify (ns sym)
  "Ensure SYM is qualified with namespace NS."
  (let* ((tpl  (ns/split-sym sym))
         (ns   (symbol-name ns))
         (name (symbol-name (cdr tpl))))
    (intern (concat ns "/" name))))


(defun ns/make-key (ns sym)
  "Make a key for the symbols table."
  (let* ((sym  (ns/qualify ns sym))
         (hash (ns/hash sym)))
    `(,hash . ,sym)))

;;; --------------------------- Table Accessors -----------------------------------

(defun ns/intern (ns sym)
  "Intern the given ns/sym into the namespace table. Returns the tuple of (hash * sym) used as the key."
  (let ((key (ns/make-key ns sym)))
    ;; Make sure existing metadata isn't lost.
    (unless (gethash key ns/symbols-table)
      (puthash key (make-ns-meta) ns/symbols-table))
    key))

(defun ns/hash-keys (table)
  (loop for k being the hash-keys of table collect k))

(defun ns/get-symbol-name (hash)
  "Return the interned symbol name corresponding to HASH, or nil if no such name is interned."
  (let ((filtered (remove-if-not (lambda (tpl) (equal hash (car tpl)))
                                 (ns/hash-keys ns/symbols-table))))
    (cdr-safe (car-safe filtered))))

(defun ns/get-symbol-hash (ns sym)
  "Return the interned symbol hash corresponding to SYM, or nil if no such hash is interned."
  (let* ((sym (ns/qualify ns sym))
         (filtered (remove-if-not (lambda (tpl) (equal sym (cdr tpl)))
                                  (ns/hash-keys ns/symbols-table))))
    (car-safe (car-safe filtered))))

(defun ns/get-symbol-meta (ns sym)
  "Gets the metadata for the given symbol, or nil no such symbol is interned."
  (gethash (ns/make-key ns sym) ns/symbols-table))


;;; --------------------------- Imports/Exports -----------------------------------

(defconst ns/imports-table (make-hash-table)
  "ns -> (hash * name) -> (hash * name)")

(defconst ns/exports-table (make-hash-table)
  "ns -> (hash * name) -> (hash * name)")

(defun ns/puthash-in (outer-key inner-key inner-value table)
  "Insert a value into a nested hash-table, creating the outer table if needed."
  (let ((tbl (gethash outer-key table)))
    (unless tbl
      (setq tbl (make-hash-table :test 'equal))
      (puthash outer-key tbl table))
    (puthash inner-key inner-value tbl)))

(defun ns/export (ns sym)
  "Make SYM publicly accessible."
  ;; Ensure metadata exists for SYM
  (ns/intern ns sym)
  (let ((tpl  (ns/make-key ns sym) )
        (meta (ns/get-symbol-meta ns sym)))
    (ns/puthash-in ns tpl tpl ns/exports-table)
    (setf (ns-meta-public? meta) t)
    tpl))

(defun ns/import (ns-from ns-to sym)
  "Import a symbol defined by one namespace into another."
  ;; Ensure SYM is publicly accessible.
  (let* ((meta    (ns/get-symbol-meta ns-from sym))
         (public? (when meta (ns-meta-public? meta))))
    (assert public? nil
            "Symbol `%s` is undefined or inaccessible from namespace `%s`"
            (ns/qualify ns-from sym) ns-to))
  ;; Add symbol to imports for NS-TO.
  (let ((tpl (ns/make-key ns-from sym)))
    (ns/puthash-in ns-to tpl tpl ns/imports-table)
    tpl))

(defun ns/import-all (from-ns into-ns)
  "Import all public symbols from one namespace into another."
  (let ((tbl (gethash from-ns ns/exports-table)))
    (when tbl
      (mapcar (lambda (tpl)
                (ns/import from-ns into-ns (cdr tpl)))
              (ns/hash-keys tbl)))))

;;; ============================== Operators ===================================

(defvar ns/current-ns 'ns)

(defun ns/hook-p (tpl)
  "Returns true if the given (hash* name) tuple points to a hook var, else nil"
  (string-match-p "-hook$" (symbol-name (cdr tpl))))

(defun ns/hook-or-fn-p (tpl)
  "Returns true if the given (hash * name) tuple resolves to a function or hook variable."
  (or (functionp (car tpl))
      (functionp (cdr tpl))
      (ns/hook-p tpl)))

(defun ns/find-public-sym (ns sym)
  "Gets the hash and name of a symbol if it is public, else nil."
  (let* ((tpl (ns/make-key ns sym))
         (meta  (ns/get-symbol-meta ns sym)))
    (when (and meta
               (ns-meta-public? meta)
               (ns/hook-or-fn-p tpl))
      tpl)))

(defun ns/find-imported-sym (unqualified-sym ns)
  "Try to find the given symbol in the imports for namespace NS.
Returns the hash and name of the sym if if it succeeds, else nil"
  (let* ((tbl     (gethash ns ns/imports-table))
         (tuples  (when tbl (ns/hash-keys tbl)))
         (match-p (lambda (tpl)
                    (let* ((name (cdr tpl))
                           (sym  (cdr (ns/split-sym name))))
                      (and (equal sym unqualified-sym)
                           (ns/hook-or-fn-p tpl))))))
    (car-safe
     (when tuples
       (remove-if-not match-p tuples)))))

(defun ns/resolve (sym)
  "Returns the hash for the given symbol, or nil if resolution fails"
  (let* ((tpl   (ns/split-sym sym))
         (ns    (or (car tpl) ns/current-ns))
         (name  (cdr tpl))
         (hash  (ns/get-symbol-hash ns name)))
    (or
     ;; Resolve from this namespace, shadowing imports.
     (when (and hash (equal ns/current-ns ns))
       (ns/make-key ns name))
     ;; Find imports or public syms.
     (ns/find-imported-sym sym ns/current-ns)
     (ns/find-public-sym ns name))))

;;; ----------------------------------------------------------------------------

(defmacro @sym (symbol)
  "Return the hashed name of SYM."
  (assert (symbolp symbol))
  (let* ((tpl  (ns/resolve symbol))
         (hash (car-safe tpl))
         (name (cdr-safe tpl))
         (ns   (car-safe (ns/split-sym name))))
    (assert tpl ()
            "Symbol `%s` is undefined or inaccessible from namespace `%s`."
            symbol ns/current-ns)
    `',(cond
        ;; Return hash for hooks.
        ((ns/hook-p tpl)
         hash)
        ;; Return names of public functions and var accessors.
        ((and (not (equal ns ns/current-ns)) (functionp name))
         name)
        ;; In all other cases, return the hash.
        (t
         hash))))


(defmacro @using (ns &rest body)
  "Dynamically rebind the current namespace to NS while evaluating BODY."
  (declare (indent 1))
  (assert (symbolp ns))
  `(let ((ns/current-ns ',ns))
     ,@body))


(defmacro @ (symbol)
  "Evaluate SYMBOL as a var in the current namespace context."
  (assert (symbolp symbol))
  (let* ((tpl  (ns/resolve symbol))
         (hash (car-safe tpl))
         (sym  (cdr-safe tpl))
         (ns   (when sym (car (ns/split-sym sym)))))
    (assert hash ()
            "Symbol `%s` is undefined or inaccessible from namespace `%s`."
            symbol ns/current-ns)
    (assert (equal ns ns/current-ns) ()
            "Invalid use of `@`. `%s` is in another namespace.
Call that symbol's accessor function instead." sym)
    hash))


(defun @dynamic (symbol)
  "Evaluate a namespace-qualified symbol dynamically."
  (eval `(@ ,symbol)))


(defmacro @call (fn &rest args)
  "Apply the given namespace-qualified function."
  (assert (symbolp fn))
  (let* ((tpl  (ns/resolve fn))
         (hash (car-safe tpl))
         (sym  (cdr-safe tpl))
         )
    (assert hash ()
            "Function `%s` is undefined or inaccessible from namespace `%s`."
            fn ns/current-ns
            )
    (assert (functionp hash) ()
            "`%s` is not a function. Use `@` to evaluate vars." sym
            )
    `(funcall ',hash ,@args)))


(defmacro @set (symbol value)
  "Set the value of a namespace-qualified symbol."
  (assert (symbolp symbol))

  (let* ((tpl  (ns/resolve symbol))
         (hash (car-safe tpl))
         (name (cdr-safe tpl)))
    (if tpl
        (let* ((tpl  (ns/split-sym name))
               (ns   (car-safe tpl))
               (sym  (cdr-safe tpl))
               (meta (ns/get-symbol-meta ns sym))
               )
          (assert (ns-meta-mutable? meta) ()
                  "Invalid use of `@set`. `%s` is immutable." name)
          (assert (equal ns ns/current-ns) ()
                  "Invalid use of `@set`. `%s` is in another namespace.
Package authors should use DEFCUSTOM for publicly mutable vars." name)
          `(setq ,hash ,value))

      ;; Could not resolve SYMBOL.
      (error "Variable `%s` is undefined or inaccessible from namespace `%s`."
             symbol ns/current-ns))))

(defmacro @lambda (args &rest body)
  "A lambda function that captures the surrounding namespace environment."
  (declare (indent defun))
  `(lambda ,args (@using ,ns/current-ns ,@body)))


;;; ================================ Definitions ===============================

(defun ns/exported-p (sym)
  (ns-meta-public? (ns/get-symbol-meta ns/current-ns sym)))

(defmacro ns/defaccessor (sym docstring)
  "Make a default accessor function for a public var."
  (let* ((name (ns/qualify ns/current-ns sym))
         (doc  (or docstring
                   (format "Auto-generated getter for %s" name)))
         (hash (ns/get-symbol-hash ns/current-ns sym)))
    `(defun ,name ()
       ,doc
       ,hash)))

;;; ----------------------------------------------------------------------------

(defmacro defn (name arglist &optional docstring &rest body)
  "Define a namespace-qualified function.
If BODY contains a call to (interactive), this will expand to `defun`. Otherwise, `defun*` is used."
  (declare (indent defun))
  (let* (
         (tpl   (ns/intern ns/current-ns name))
         (hash  (car tpl))
         (qual  (cdr tpl))
         (body  body)
         (doc   docstring)
         ;; Tolerate body forms in the `docstring` position.
         (forms (cond
                 ((and (stringp doc) body) (list* doc nil body))
                 (body (list* nil doc body))
                 (t (list nil doc))))
         ;; Extract parts.
         (docstring    (or (first forms) ""))
         (interactive? (lambda (s) (equalp (car-safe s) 'interactive)))
         (interactive  (find-if interactive? forms))
         (body         (remove-if interactive? (rest forms)))
         (defun-form   (if interactive 'defun 'defun*))
         )
    `(progn
       (,defun-form ,hash ,arglist
         ,docstring
         ,interactive
         (@using ,ns/current-ns ,@body))

       ,(when (ns/exported-p qual)
          `(defalias ',qual ',hash))

       ',qual)))


(defmacro def (symbol value &optional docstring)
  "Define SYMBOL as an immutable var in the current namespace. Otherwise identical to `defconst`."
  (assert (symbolp symbol))
  (let* ((tpl  (ns/intern ns/current-ns symbol))
         (hash (car tpl))
         (name (cdr tpl))
         (qual (ns/qualify ns/current-ns name))
         )
    ;; Ensure this is now an immutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) nil)

    `(progn
       (defconst ,hash ,value ,docstring)

       ,(when (and (ns/exported-p qual)
                   (not (fboundp qual)))
          `(ns/defaccessor ,qual ,docstring))

       ',name)))


(defmacro defmutable (symbol &optional value docstring)
  "Define SYMBOL as a mutable var in the current namespace. Otherwise identical to `defvar`."
  (assert (symbolp symbol))
  (let* ((tpl  (ns/intern ns/current-ns symbol))
         (hash (car tpl))
         (name (cdr tpl))
         (qual (ns/qualify ns/current-ns name))
         )
    ;; Ensure this is now a mutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) t)

    `(progn
       (defvar ,hash ,value ,docstring)

       ,(when (and (ns/exported-p qual)
                   (not (fboundp qual)))
          `(ns/defaccessor ,qual ,docstring))

       ',name)))


;;; ================================ Namespace =================================

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


;;; ----------------------------------------------------------------------------

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

AUTOLOADING
-----------
A LOAD FORM represents an item that will be autoloaded. It is either
  1. a symbol
  2. a list of the form (sym &key interactive type docstring) that will be passed to the Emacs autoload function. See the documentation for AUTOLOAD for an explanation of these symbols.
"
  (declare (indent 1))
  (assert (symbolp name))
  (assert (not (string-match-p "/" (symbol-name name))) ()
          "Invalid namespace identifier: `%s`
Forward-slashes (`/`) cannot be used." name)
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
    (mapc (ns/destructure-dep (lambda (x xs) (ns/handle-use ns/base-path x xs)))
          use))
  ;; Rebind the current namespace.
  (setq ns/current-ns name)
  `(provide ',name))


;;; ================================ Font Lock =================================

(defn match-identifier-after (&rest strings)
  (rx-to-string `(and "("
                      (or ,@strings) (+ space)
                      (group (+ (not (any space "()[]")))))))

(def match-kw    (rx "(" (group (or "defn" "def" "defmutable" "namespace" "@using" "@lambda")) space))
(def match-op    (rx "(" (group (or "@" "@dynamic" "@call" "@sym" "@set")) space))
(def match-fname (@call match-identifier-after "defn"))
(def match-var   (@call match-identifier-after "def" "defmutable"))
(def match-ns    (@call match-identifier-after "namespace" "@using"))

(add-hook 'emacs-lisp-mode-hook
          (@lambda ()
            (font-lock-add-keywords
             nil
             `((,(@ match-kw)    1 font-lock-keyword-face)
               (,(@ match-op)    1 font-lock-keyword-face)
               (,(@ match-fname) 1 font-lock-function-name-face)
               (,(@ match-var)   1 font-lock-variable-name-face)
               (,(@ match-ns)    1 font-lock-constant-face)))))

;;; ============================================================================

(namespace user)
(provide 'namespaces)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; namespaces.el ends here
