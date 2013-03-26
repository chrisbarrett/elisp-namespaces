;;; namespaces.el --- An implementation of namespaces for Elisp, with an emphasis on immutabilty.
;;
;; Author: Chris Barrett
;; URL: https://github.com/chrisbarrett/elisp-namespaces
;; Version: 1.2.2
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

(require 'cl-lib)
(autoload 'package-installed-p "package")
(autoload 'package-install "package")

;;; ================================ Core ======================================

(cl-defstruct ns-meta
  public?
  mutable?)

(cl-defstruct ns-symbol
  name
  hash
  ns)

(defconst ns/symbols-table (make-hash-table :test 'equal)
  "ns-symbol -> ns-meta")

;;; --------------------------- Symbol Processing ---------------------------------

(defun ns/hash (ns name)
  "Hash a symbol for insertion into lookup tables."
  (intern (format "%s--internal-%s" ns name)))

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
  (let ((qual (ns/qualify ns sym))
        (name (cdr (ns/split-sym sym))))
    (make-ns-symbol :name qual
                    :hash (ns/hash ns name)
                    :ns ns)))

;;; --------------------------- Table Accessors -----------------------------------

(defun ns/intern (ns sym)
  "Intern the given ns/sym into the namespace table. Returns the
`ns-symbol' used as the key."
  (let ((key (ns/make-key ns sym)))
    ;; Make sure existing metadata isn't lost.
    (unless (gethash key ns/symbols-table)
      (puthash key (make-ns-meta) ns/symbols-table))
    key))

(defun ns/hash-keys (table)
  (cl-loop for k being the hash-keys of table collect k))

(defun ns/get-symbol-name (hash)
  "Return the interned symbol name corresponding to HASH, or nil
if no such name is interned."
  (let ((nsym (cl-find-if (lambda (x) (equal hash (ns-symbol-hash x)))
                          (ns/hash-keys ns/symbols-table))))
    (when nsym (ns-symbol-name nsym))))

(defun ns/get-symbol-hash (ns sym)
  "Return the interned symbol hash corresponding to SYM, or nil
if no such hash is interned."
  (let* ((qual (ns/qualify ns sym))
         (nsym (cl-find-if (lambda (x) (equal qual (ns-symbol-name x)))
                           (ns/hash-keys ns/symbols-table))))
    (when nsym (ns-symbol-hash nsym))))

(defun ns/get-symbol-meta (ns sym)
  "Gets the metadata for the given symbol, or nil if no such
symbol is interned."
  (gethash (ns/make-key ns sym) ns/symbols-table))

(defun ns/get-ns-symbol-meta (nsym)
  "Gets the metadata for the given symbol, or nil if no such
symbol is interned."
  (gethash nsym ns/symbols-table))

;;; --------------------------- Imports/Exports -----------------------------------

(defconst ns/imports-table (make-hash-table)
  "A hashtable of hashtables, mapping namespaces to their imports.
ns -> (ns-symbol -> ns-symbol)")

(defconst ns/exports-table (make-hash-table)
  "A hashtable of hashtables, mapping namespaces to their exports.
ns -> (ns-symbol -> ns-symbol)")

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

  (let ((nsym  (ns/make-key ns sym))
        (meta (ns/get-symbol-meta ns sym)))
    (ns/puthash-in ns nsym nsym ns/exports-table)
    (setf (ns-meta-public? meta) t)
    nsym))

(defun ns/import (ns-from ns-to sym)
  "Import a symbol defined by one namespace into another."

  ;; Ensure SYM is publicly accessible.
  (let* ((meta    (ns/get-symbol-meta ns-from sym))
         (public? (when meta (ns-meta-public? meta))))
    (cl-assert public? ()
               "Symbol `%s` is not public or is undefined."
               (ns/qualify ns-from sym)))

  ;; Add symbol to imports for NS-TO.
  (let ((nsym (ns/make-key ns-from sym)))
    (ns/puthash-in ns-to nsym nsym ns/imports-table)
    nsym))

(defun ns/import-all (from-ns into-ns)
  "Import all public symbols from one namespace into another."
  (let ((tbl (gethash from-ns ns/exports-table)))
    (when tbl
      (mapcar (lambda (nsym)
                (ns/import from-ns into-ns (ns-symbol-name nsym)))
              (ns/hash-keys tbl)))))

;;; ============================== Operators ===================================

(defvar ns/current-ns 'user
  "Represents the current namespace for symbol resolution and definitions.")

(make-variable-buffer-local 'ns/current-ns)

(defun ns/fn-p (nsym)
  "Returns true if the given ns-symbol resolves to a function."
  (or (functionp (ns-symbol-name nsym))
      (functionp (ns-symbol-hash nsym))))

(defun ns/defined-local-p (ns hash)
  "Returns true if HASH is a defined symbol in namespace NS."
  (and (equal ns ns/current-ns) hash (boundp hash)))

(defun ns/find-imported-sym (ns unqualified-sym)
  "Try to find the given symbol in the imports for namespace NS.
Returns the hash and name of the sym if if it succeeds, else nil"
  (let* ((table   (gethash ns ns/imports-table))
         (symbols (when table (ns/hash-keys table))))
    (cl-find-if (lambda (nsym)
                  (let ((name (ns-symbol-name nsym)))
                    (and (ns/fn-p nsym)
                         (equal unqualified-sym (cdr (ns/split-sym name)))
                         name)))
                symbols)))

(defun ns/resolve (sym)
  "Returns the hash for the given symbol, or nil if resolution fails"
  (let* ((tpl  (ns/split-sym sym))
         (ns   (or (car tpl) ns/current-ns))
         (name (cdr tpl))
         (hash (ns/get-symbol-hash ns name)))
    (or
     ;; Resolve from this namespace, shadowing imports.
     (when (ns/defined-local-p ns hash)
       (ns/make-key ns name))

     ;; Resolve from imports.
     (ns/find-imported-sym ns/current-ns sym)

     ;; Use the supplied form.
     (ns/make-key ns name))))

;;; ----------------------------------------------------------------------------

(defmacro ~ (symbol)
  "Return the hashed name of SYM."
  (cl-assert (symbolp symbol))
  (let* ((nsym (ns/resolve symbol))
         (name (ns-symbol-name nsym))
         (ns   (car (ns/split-sym name))))

    ;; Return names of public functions and var accessors.
    ;; In all other cases, return the hash.
    `',(if (and (not (equal ns ns/current-ns))
                (functionp name))
           name
         (ns-symbol-hash nsym))))

(defmacro in-ns (ns &rest body)
  "Dynamically rebind the current namespace to NS while evaluating BODY."
  (declare (indent 1))
  (cl-assert (symbolp ns))
  `(let ((ns/current-ns ',ns))
     (eval '(progn ,@body))))

(defmacro @ (symbol)
  "Evaluate SYMBOL as a var in the current namespace context."
  (cl-assert (symbolp symbol))
  (let* ((nsym (ns/resolve symbol))
         (hash (ns-symbol-hash nsym))
         (sym  (ns-symbol-name nsym))
         (ns   (when sym (car (ns/split-sym sym)))))

    `(progn

       (cl-assert ,hash ()
                  "Symbol `%s' is undefined or inaccessible from namespace `%s`."
                  ',symbol ns/current-ns)

       (cl-assert (equal ',ns ns/current-ns) ()
                  "Invalid use of `@`. `%s' is in another namespace.
Call that symbol's accessor function instead." ,sym)

       ,hash)))

(defmacro _ (fn &rest args)
  "Apply the given namespace-qualified function."
  (cl-assert (symbolp fn))
  (let* ((nsym (ns/resolve fn))
         (hash (ns-symbol-hash nsym))
         (sym  (ns-symbol-name nsym))
         (meta (ns/get-ns-symbol-meta nsym)))

    ;; Dynamically resolve function.
    `(cond
      ((fboundp ',sym)  (,sym  ,@args))
      ((and (fboundp ',hash) (equal (ns-symbol-ns ,nsym) ns/current-ns)) (,hash ,@args))
      ((and (fboundp ',hash) (ns-meta-public? ,meta)) (,hash ,@args))
      ((boundp  ',hash) (error "`%s' is a var, not a function" ',sym))
      ((fboundp ',hash) (error "The function `%s' is inaccessible from namespace '%s'"
                               ',sym ns/current-ns))
      (t                (error "The function `%s' is not defined" ',sym)))))

(defmacro @set (symbol value)
  "Set the value of a namespace-qualified symbol."
  (cl-assert (symbolp symbol))

  (let* ((nsym (ns/resolve symbol))
         (hash (ns-symbol-hash nsym))
         (name (ns/split-sym (ns-symbol-name nsym)))
         (ns   (car name))
         (sym  (cdr name))
         (meta (ns/get-symbol-meta ns sym)))

    ;; Ensure the value mutable, then set value. We must do this
    ;; dynamically because the var could be redefined as mutable at runtime.
    `(progn
       (cl-assert (ns-meta-mutable? ,meta) ()
                  "Invalid use of `@set`. `%s` is immutable." ,sym)

       (cl-assert (equal ',ns ns/current-ns) ()
                  "Invalid use of `@set`. `%s` is in another namespace." ,sym)

       (setq ,hash ,value))))

(defmacro lambda- (args &rest body)
  "A `lambda' function that captures the surrounding namespace environment."
  (declare (indent defun))
  `(lambda ,args (in-ns ,ns/current-ns ,@body)))

(defmacro @eval-after-load (feature &rest body)
  "A version of `eval-after-load' that captures the declaring namespace in BODY."
  (declare (indent defun))

  `(eval-after-load ,feature
     '(in-ns ,ns/current-ns
        ,@body)))

;;; ================================ Definitions ===============================

(defun ns/exported-p (sym)
  (ns-meta-public? (ns/get-symbol-meta ns/current-ns sym)))

(defmacro ns/defaccessor (sym docstring)
  "Make a default accessor function for a public var."
  (let* ((name (ns/qualify ns/current-ns sym))
         (doc  (or docstring (format "Auto-generated getter for %s" name)))
         (hash (ns/get-symbol-hash ns/current-ns sym)))

    `(defun ,name ()
       ,doc
       ,hash)))

;;; ----------------------------------------------------------------------------

(defmacro defn (name arglist &optional docstring &rest body)
  "Define a namespace-qualified function.
If BODY contains a call to (interactive), this will expand to
`defun'. Otherwise it will expand to `cl-defun', permitting a
Common Lisp-style arglist."
  (declare (indent defun) (doc-string 3))
  (cl-assert (symbolp name))
  (cl-assert (listp arglist))

  (let* ((nsym (ns/intern ns/current-ns name))
         (hash (ns-symbol-hash nsym))
         (qual (ns-symbol-name nsym))
         (body body)
         (doc  docstring)

         ;; Handle body forms in the `docstring` position.
         (forms (cond ((and (stringp doc) body) (cl-list* doc nil body))
                      (body                     (cl-list* nil doc body))
                      (t                        (list nil doc))))
         (docstring (or (car forms) ""))

         ;; Use DEFUN if the body contains INTERACTIVE, else use CL-DEFUN.
         (interactive? (lambda (s) (equal (car-safe s) 'interactive)))
         (interactive  (cl-find-if interactive? forms))
         (body         (cl-remove-if interactive? (cdr forms)))
         (defun-form   (if interactive 'defun 'cl-defun)))

    `(progn
       ;; Define the function.
       (,defun-form ,hash ,arglist
         ,docstring
         ,interactive
         (in-ns ,ns/current-ns ,@body))

       ;; 'Export' this function by creating an alias to the mangled name.
       ,(when (ns/exported-p qual)
          `(defalias ',qual ',hash))

       ',qual)))

(defmacro def (symbol value &optional docstring)
  "Define SYMBOL as an immutable var in the current
namespace. Otherwise identical to `defconst'."
  (declare (doc-string 3))
  (cl-assert (symbolp symbol))

  (let* ((nsym (ns/intern ns/current-ns symbol))
         (hash (ns-symbol-hash nsym))
         (name (ns-symbol-name nsym))
         (qual (ns/qualify ns/current-ns name)))

    ;; Ensure this is now an immutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) nil)

    `(progn
       ;; Declare the variable.
       (defconst ,hash ,value ,docstring)

       ;; Export the var by creating an accessor function (unless one
       ;; already exists).
       ,(when (and (ns/exported-p qual)
                   (not (fboundp qual)))
          `(ns/defaccessor ,qual ,docstring))

       ',name)))

(defmacro defmutable (symbol &optional value docstring)
  "Define SYMBOL as a mutable var in the current
namespace. Otherwise identical to `defvar`."
  (declare (doc-string 3))
  (cl-assert (symbolp symbol))

  (let* ((nsym (ns/intern ns/current-ns symbol))
         (hash (ns-symbol-hash nsym))
         (name (ns-symbol-name nsym))
         (qual (ns/qualify ns/current-ns name)))

    ;; Ensure this is now a mutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) t)

    `(progn
       ;; Declare the variable.
       (defvar ,hash ,value ,docstring)

       ;; Export the var by creating an accessor function (unless one
       ;; already exists).
       ,(when (and (ns/exported-p qual)
                   (not (fboundp qual)))
          `(ns/defaccessor ,qual ,docstring))

       ',name)))

;;; ================================ Namespace =================================

(cl-defun ns/delete-nth (nth xs &key (count 1))
  (cl-delete-if (lambda (_) t) xs :start nth :count count))

(defun ns/delete-keyword-and-arg (key xs)
  (let ((pos (cl-position key xs)))
    (if pos
        (ns/delete-nth pos xs :count 2)
      xs)))

(defun ns/destructure-dep (handler)
  "Calls HANDLER to load the dependency form, provided the :when and :unless keywords do not override this."
  (declare (indent 1))
  `(lambda (dep)
     (cl-destructuring-bind
         (sym &rest autos &key (when t) (unless nil) &allow-other-keys)
         (if (sequencep dep) dep (list dep))

       (let* ((w     (if (symbolp when)   when   (eval when)))
              (u     (if (symbolp unless) unless (eval unless)))
              (autos (ns/delete-keyword-and-arg :when autos))
              (autos (ns/delete-keyword-and-arg :unless autos)))

         (when (and w (not u))
           (,handler sym autos))))))

(defun ns/handle-import (from-ns into-ns deps)
  "Load dependencies. If DEPS is empty, load all symbols exported by FROM-NS."
  (if deps
      (cl-loop for sym in deps do (ns/import from-ns into-ns sym))
    (ns/import-all from-ns into-ns)))

(defun ns/autoload-dep (feature dep)
  "Autoload symbols from FEATURE, where DEP is a list of symbols to autoload."
  (let ((feat (or (when (stringp feature) feature)
                  (symbol-file feature)
                  (symbol-name feature))))
    (if (symbolp dep)
        (autoload dep feat)
      (cl-destructuring-bind (symbol &key docstring interactive type
                                     &allow-other-keys) dep
        (autoload symbol feat docstring interactive type)))))

(defun ns/handle-use (feature autoloads)
  "Autoload the specified symbols from namespace or feature FEATURE."
  (if autoloads
      (cl-loop for dep in autoloads do (ns/autoload-dep feature dep))
    (require feature)))

(defun ns/handle-pkg (pkg autoloads)
  "Require a package from an online repository, downloading it if needed."
  (unless (package-installed-p pkg)
    (package-install pkg))
  (if autoloads
      (cl-loop for dep in autoloads collect (ns/autoload-dep pkg dep))
    (require pkg)))

;;; ----------------------------------------------------------------------------

(cl-defmacro namespace (name &key import export use packages)
  "Define or reopen an existing namespace.
The keyword agruments allow you to define exports, imports and
dependencies on Emacs features and online packages.

NAME should be a symbol used to identify the namespace and
qualify its members.

OPTIONAL KEYWORD ARGUMENTS
--------------------------

IMPORT is a vector of the form [ dep ... ], where each `dep` is
  either:

1. a namespace whose members will be all be imported

2. a list of the form (ns & syms ... ), where `ns` is a namespace
  identifier and each sym is a member to import.

EXPORT is a vector of the form [ sym ... ], where each `sym` is a
symbol to make publicly accessible.

PACKAGES is a vector of the form [ dep ... ], where each `dep` is
either:

  1. a package to be downloaded and loaded

  2. a list of the form (pkg & load ... ), where `pkg` is the
  package name and each `load` is a LOAD FORM as described later.

USE is a vector of the form [ dep ... ], where each `dep` is either:

  1. an emacs feature to require

  2. a list of the form (feature & load ... ) where `feature` is
  an Emacs feature and each `load` is a LOAD FORM, described
  below.

AUTOLOADING
-----------
A LOAD FORM represents an item that will
be autoloaded. It is either

1. a symbol

2. a list of the form (sym &key interactive type docstring) that
will be passed to the Emacs autoload function. See the
documentation for `autoload' for an explanation of these symbols.
"
  (declare (indent 1))
  (cl-assert (symbolp name))
  (cl-assert (not (string-match-p "/" (symbol-name name))) ()
             "Invalid namespace identifier: `%s`. Forward-slashes (`/`) cannot be used." name)

  ;; Set ns at macro-expansion-time.
  (setq ns/current-ns name)

  (let ((ns (cl-gensym)))
    `(let ((,ns ',name))

       ;; Export the given symbols.
       (mapc (lambda (x) (ns/export ,ns x))
             ,export)

       ;; Import the given symbols from other namespaces.
       (mapc (ns/destructure-dep (lambda (x xs) (ns/handle-import x ,ns xs)))
             ,import)

       ;; download and load packages.
       (mapc (ns/destructure-dep (lambda (x xs) (ns/handle-pkg x xs)))
             ,packages)

       ;; Load emacs features and files.
       (mapc (ns/destructure-dep (lambda (x xs) (ns/handle-use x xs)))
             ,use)

       ;; Set ns at runtime.
       (setq ns/current-ns ,ns)
       (provide ,ns))))

;;; ================================ Font Lock =================================

(defun ns/match-identifier-after (&rest strings)
  (rx-to-string `(and "("
                      (or ,@strings) (+ space)
                      (group (+ (not (any space "()[]")))))))

(defconst ns/match-kw    (rx "("
                             (group (or "defn" "def" "defmutable" "namespace"
                                        "in-ns" "lambda-" "@eval-after-load"))
                             word-end))

(defconst ns/match-op    (rx "(" (group (or "@" "_" "~" "@set")) space))
(defconst ns/match-fname (ns/match-identifier-after "defn"))
(defconst ns/match-var   (ns/match-identifier-after "def" "defmutable"))
(defconst ns/match-ns    (ns/match-identifier-after "namespace" "in-ns"))


(defun ns/apply-font-lock ()
  (font-lock-add-keywords
   nil
   `((,ns/match-kw    1 font-lock-keyword-face)
     (,ns/match-op    1 font-lock-keyword-face)
     (,ns/match-fname 1 font-lock-function-name-face)
     (,ns/match-var   1 font-lock-variable-name-face)
     (,ns/match-ns    1 font-lock-constant-face))))

(add-hook 'emacs-lisp-mode-hook 'ns/apply-font-lock)

;;; ============================================================================

(provide 'namespaces)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; namespaces.el ends here
