;;; NAMESPACES
;;;
;;; Copyright (c) 2012, Chris Barrett
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright notice, this
;;;    list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;
;;; DESCRIPTION:
;;; A straight-forward implementation of namespaces for Emacs LISP.
;;;
;;; Helps you keep the global namespace clean and protect your functions from
;;; prying REPLs.
;;;
;;; Requires Emacs 24 or later.
;;;
;;;
;;; INSTALLATION:
;;; Put this file in your load path and (require 'namespaces).
;;;

(setq lexical-binding t)
(require 'cl)
(require 'package)

(defvar *ns* '__ns
  "Defines the current namespace. Use the `namespace` macro to open a namespace.")

(defvar *ns-base-path* (concat user-emacs-directory "elisp/")
  "Defines the base directory for namespace resolution.")


;;; -------------------------- Symbol Mapping ----------------------------------

(defconst __ns/hashes->symbols-table (make-hash-table)
  "Maps namespace hashes to their proper symbols.")

(defconst __ns/symbols->hashes-table (make-hash-table)
  "Maps namespace symbols to their hashed values.")

(defconst __ns/exports-table (make-hash-table)
  "Maps namespaces to their publicly accessible functions.")


(defun __ns/hash (ns sym)
  "Hash a symbol for insertion into lookup tables."
  (let ((s (concat (symbol-name ns) (symbol-name sym))))
    (intern (format "__ns/namespaced_sym__%s" (secure-hash 'sha256 s)))))


(defun __ns/hash->symbol (ns hash)
  "Retrieve the symbol corresponding to the given hash if it exists in namespace NS, or nil."
  (let ((table (gethash ns __ns/hashes->symbols-table)))
    (when table
      (gethash hash table))))


(defun __ns/symbol->hash (ns sym)
  "Retrieve the hashed value for the given symbol if it exists in namespace NS, or nil."
  (let ((table (gethash ns __ns/symbols->hashes-table)))
    (when table
      (gethash sym table))))


(defun* __ns/insert-into (table ns (key value))
  "Insert a symbol into the specified namespace table, creating that table if it does not already exist.."
  (let ((tbl  (gethash ns table)))
    (unless tbl
      (setq tbl (make-hash-table))
      (puthash ns tbl table))
    ;; Add symbol to table.
    (puthash key value tbl)))


(defun __ns/intern (ns-from ns-into sym)
  "Intern a symbol for the given namespace. Returns the hash for that symbol for later retrieval."
  (let ((hash (__ns/hash ns-from sym)))
    (__ns/insert-into __ns/hashes->symbols-table ns-into (list hash sym))
    (__ns/insert-into __ns/symbols->hashes-table ns-into (list sym  hash))))


(defun __ns/export (ns sym)
  "Export a symbol declared by namespace NS."
  (__ns/insert-into __ns/exports-table ns (list sym (__ns/hash ns sym))))


(defun __ns/get-exports (ns)
  "Get the symbols exported by namespace NS."
  (gethash ns __ns/exports-table))


(defun __ns/import (ns-from ns-into sym)
  "Import a symbol from one namespace into another."
  (let* ((exports (gethash ns-from __ns/exports-table))
         (exported? (ignore-errors (gethash sym exports))))
    (assert (not (null exported?)) nil
            "Symbol `%s` does not exist in namespace `%s`." sym ns-from)
    (__ns/intern ns-from ns-into sym)))


(defun __ns/import-all (ns-from ns-into)
  "Import all symbols from one namespace into another."
  (let ((tbl (__ns/get-exports ns-from)))
    (when tbl
      (loop for sym being the hash-keys in tbl
            collect (__ns/import ns-from ns-into sym)))))


;;; -------------------------- Core operators ----------------------------------

(defun __ns/split-sym (sym)
  "Extract the namespace from a symbol, or apply the current namespace.
foo     -> (*ns* foo)
foo/bar -> (foo bar)"
  (let* ((xs (split-string (symbol-name sym) "/"))
         (s1 (intern (car xs)))
         (s2 (when (nth 1 xs) (intern (nth 1 xs)))))
    (if (< 1 (length xs))
        `(,s1 ,s2)
      `(,*ns* ,s1))))


(defmacro @sym (symbol)
  "Return the hashed name of SYM."
  (assert (symbolp symbol))
  (let* ((xs  (__ns/split-sym symbol))
         (ns  (nth 0 xs))
         (sym (nth 1 xs))
         (hash (__ns/symbol->hash ns sym)))
    `',hash))


(defmacro @using (ns &rest body)
  "Dynamically rebind the current namespace to NS while evaluating BODY."
  (declare (indent 1))
  (assert (symbolp ns))
  `(let ((*ns* ',ns))
     ,@body))


(defmacro @ (symbol)
  "Evaluate a literal as a qualified symbol in the current namespace."
  (assert (symbolp symbol))
  (let ((sym (eval `(@sym ,symbol))))
    (assert (not (null sym)) nil
            "No variable named `%s` is accessible from namespace `%s`." symbol *ns*)
    `(@using ,*ns*
         (eval (@sym ,symbol)))))


(defun @dynamic (symbol)
  "Evaluate a namespace-qualified symbol dynamically."
  (eval `(@ ,symbol)))


(defmacro @call (fn &rest args)
  "Apply the given namespace-qualified function."
  (assert (symbolp fn))
  (let ((sym (eval `(@sym ,fn))))
    (assert (functionp sym) nil
            "No function named `%s` is accessible from namespace `%s`." fn *ns*)
    `(funcall `,(@sym ,fn) ,@args)))


(defmacro* @set (symbol value)
  "Set the value of a namespace-qualified symbol."
  (assert (symbolp symbol))
  `(set (@sym ,symbol) ,value))


(defmacro @lambda (args &rest body)
  "A lambda function that captures the surrounding namespace environment."
  (declare (indent defun))
  `(lambda ,args (@using ,*ns* ,@body)))


;;; -------------------------- Binding macros ----------------------------------


(defmacro def (symbol &optional value docstring)
  "Define SYMBOL as a variable in the current namespace. Otherwise identical to `defvar`."
  (assert (symbolp symbol))
  (let ((name (__ns/intern *ns* *ns* symbol)))
    `(defvar ,name ,value ,docstring)))


(defmacro defn (name arglist &optional docstring &rest body)
  "Define a namespace-qualified function.
If BODY contains a call to (interactive), this will expand to `defun`. Otherwise, `defun*` is used."
  (declare (indent defun))

  (flet   ((interactive-p (s) (equalp (car-safe s) 'interactive)))
    (let* ((name  (__ns/intern *ns* *ns* name))
           (body  body)
           (doc   docstring)
           ;; Tolerate body forms in the `docstring` position.
           (forms (cond
                   ((and (stringp doc) body) (list* doc nil body))
                   (body (list* nil doc body))
                   (t (list nil doc))))
           ;; Extract parts.
           (docstring   (or (first forms) ""))
           (interactive (find-if #'interactive-p forms))
           (body        (remove-if #'interactive-p (rest forms)))
           (defun-form  (if interactive 'defun 'defun*))
           )
      `(,defun-form ,name ,arglist
         ,docstring
         ,interactive
         (@using ,*ns*
           ,@body)))))


;;; -------------------------- Namespace definition ----------------------------

(defmacro* namespace (name &key import export use packages)
  (declare (indent 1))
  (@using __ns
          ;; Export the given symbols.
          (mapc (lambda (x) (__ns/export name x))
                export)
          ;; Import the given symbols from other namespaces.
          (mapc (@call destructure-dep (lambda (x xs) (@call handle-import x name xs)))
                import)
          ;; download and load packages.
          (mapc (@call destructure-dep (lambda (x xs) (@call handle-pkg x xs)))
                packages)
          ;; Load emacs features and files.
          (mapc (@call destructure-dep (lambda (x xs) (@call handle-use *ns-base-path* x xs)))
                use))
  ;; Rebind the current namespace.
  (setq *ns* name)
  `',name)


(defn delete-nth (nth xs &key (count 1))
  (delete-if (lambda (_) t) xs :start nth :count count))

(defn delete-keyword-and-arg (key xs)
  (let ((pos (position key xs)))
    (if pos
        (@call delete-nth pos xs :count 2)
      xs)))

(defn destructure-dep (handler)
  "Calls HANDLER to load the dependency form, provided the :when and :unless keywords do not override this.."
  (declare (indent 1))
  `(lambda (dep)
     (destructuring-bind
         (sym &rest autos &key (when t) (unless nil) &allow-other-keys)
         (if (sequencep dep) dep (list dep))
       (let* (
              (w     (if (symbolp when) when (eval when)))
              (u     (if (symbolp unless) unless (eval unless)))
              (autos (@call delete-keyword-and-arg :when autos))
              (autos (@call delete-keyword-and-arg :unless autos))
             )
         (when (and w (not u))
           (funcall ,handler sym autos))))))


(defn handle-import (from-ns into-ns deps)
  "Load dependencies. If dependenices is empty, load all symbols exported by FROM-NS."
  (if deps
      (loop for sym in deps do (__ns/import from-ns into-ns sym))
    (__ns/import-all from-ns into-ns)))


(defn autoload-dep (feature dep)
  "Autoload symbols from FEATURE, where DEP is a list of symbols to autoload."
  (let ((feat (or (when (stringp feature) feature)
                  (symbol-file feature) (symbol-name feature))))
    (if (symbolp dep)
        (autoload dep feat)
      (destructuring-bind (symbol &key docstring interactive type
                                  &allow-other-keys)
          dep
        (autoload symbol feat docstring interactive type)))))


(defn join-dirs (&rest directories)
  (reduce (lambda (l r) (file-name-as-directory (concat l r)))
          directories))

(defn ns->file (base ns)
  "Return a relative filepath for a given namespace."
  (let* ((xs   (split-string (symbol-name ns) "[.]"))
         (path (apply (@sym join-dirs) base xs))
         (path (substring path 0 -1)))
    (when xs
      (concat path ".el"))))


(defn handle-use (base feature autoloads)
  "Autoload the specified symbols from namespace or feature FEATURE."
  (let* ((path (@call ns->file base feature))
         (file (when (file-exists-p path) path)))
    (cond
     (autoloads
      (loop for dep in autoloads collect (@call autoload-dep (or file feature) dep)))
     (file
      (load file))
     (t
      (require feature)))))


(defn handle-pkg (pkg autoloads)
  "Require a package from an online repository, downloading it if needed."
  (unless (package-installed-p pkg)
    (package-install pkg))
  (if autoloads
      (loop for dep in autoloads collect (@call autoload-dep pkg dep))
    (require pkg)))

;;; -------------------------- Font Lock ---------------------------------------

(namespace __ns)

(def match-kw
     (rx "(" (group
              (or (and "def" (* (any graphic)))
                  "namespace" "@using" "@lambda"))))

(defn match-identifier-after (&rest strings)
  (rx-to-string `(and (or ,@strings) (+ space)
                      (group (+ (not (any space "()[]")))))))

(def match-op    (rx "(" (group (or "@" "@dynamic" "@call" "@sym" "@set")) space))
(def match-fname (@call match-identifier-after "defn"))
(def match-var   (@call match-identifier-after "def"))
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


;;; -------------------------- Public utilities -------------------------------

(namespace ns-utils
  :export
  [ exported-symbols
    symbol-tables
    namespaces ])


(defn hash-keys (table)
  (loop for k being the hash-keys of table collect k))

(defn hash-values (table)
  (loop for v being the hash-values of table collect v))

(defn map-hash2 (fn table)
  "Map over the key-value pairs in TABLE, collecting the results of FN."
  (loop for k being the hash-keys of table
        using (hash-key v)
        collect (eval `(funcall #',fn k v))))


(defn namespaces ()
  "Returns a list of the namespaces that have been defined."
  (@call hash-keys __ns/symbols->hashes-table))


(defn exported-symbols (ns)
  "Returns the symbols exported by the given namespace, or the current namespace if none is supplied."
  (let ((tbl (gethash ns __ns/exports-table)))
    (when tbl (@call hash-keys tbl))))


(defn symbol-tables ()
  "Returns a list of all namespaces and their symbols."
  (@call map-hash2 (lambda (k v) `(,k ,(@call hash-keys v)))
         __ns/symbols->hashes-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(namespace user)
(provide 'namespaces)


;; Local Variables:
;; no-byte-compile: t
;; End:
