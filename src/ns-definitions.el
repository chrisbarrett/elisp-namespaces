;; NS-DEFINITIONS
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
;; Macros for definining namespaces and namespace members.
;;

(provide 'ns-definitions)
(require 'ns-core)
(require 'ns-operators)
(eval-when-compile (require 'cl))
(setq lexical-binding t)


(defmacro ^def (symbol value &optional docstring)
  "Define SYMBOL as an immutable var in the current namespace. Otherwise identical to `defconst`."
  (assert (symbolp symbol))
  (let* ((tpl  (ns/intern ns/current-ns symbol))
         (hash (car tpl))
         (name (cdr tpl)))
    ;; Ensure this is now an immutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) nil)
    `(progn
       (defconst ,hash ,value ,docstring)
       ',name)))


(defmacro ^defmutable (symbol &optional value docstring)
  "Define SYMBOL as a mutable var in the current namespace. Otherwise identical to `defvar`."
  (assert (symbolp symbol))
  (let* ((tpl  (ns/intern ns/current-ns symbol))
         (hash (car tpl))
         (name (cdr tpl)))
    ;; Ensure this is now a mutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) t)
    `(progn
       (defvar ,hash ,value ,docstring)
       ',name)))


(defmacro ^defn (name arglist &optional docstring &rest body)
  "Define a namespace-qualified function.
If BODY contains a call to (interactive), this will expand to `defun`. Otherwise, `defun*` is used."
  (declare (indent defun))

  (flet   ((interactive-p (s) (equalp (car-safe s) 'interactive)))
    (let* ((tpl   (ns/intern ns/current-ns name))
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
           (docstring   (or (first forms) ""))
           (interactive (find-if #'interactive-p forms))
           (body        (remove-if #'interactive-p (rest forms)))
           (defun-form  (if interactive 'defun 'defun*))
           )
      `(progn
         (,defun-form ,hash ,arglist
           ,docstring
           ,interactive
           (^using ,ns/current-ns ,@body))
         ',qual))))


;;; -------------------------- Namespace Definition ----------------------------

(^using ns

  (defmacro* ^namespace (name &key import export use packages)
    (declare (indent 1))
    (^using ns
      ;; Export the given symbols.
      (mapc (lambda (x) (ns/export name x))
            export)
      ;; Import the given symbols from other namespaces.
      (mapc (^call destructure-dep (lambda (x xs) (^call handle-import x name xs)))
            import)
      ;; download and load packages.
      (mapc (^call destructure-dep (lambda (x xs) (^call handle-pkg x xs)))
            packages)
      ;; Load emacs features and files.
      (mapc (^call destructure-dep (lambda (x xs) (^call handle-use *ns-base-path* x xs)))
            use))
    ;; Rebind the current namespace.
    (setq ns/current-ns name)
    `',name)


  (^defn delete-nth (nth xs &key (count 1))
    (delete-if (lambda (x) t) xs :start nth :count count))

  (^defn delete-keyword-and-arg (key xs)
    (let ((pos (position key xs)))
      (if pos
          (^call delete-nth pos xs :count 2)
        xs)))

  (^defn destructure-dep (handler)
    "Calls HANDLER to load the dependency form, provided the :when and :unless keywords do not override this.."
    (declare (indent 1))
    `(lambda (dep)
       (destructuring-bind
           (sym &rest autos &key (when t) (unless nil) &allow-other-keys)
           (if (sequencep dep) dep (list dep))
         (let* (
                (w     (if (symbolp when) when (eval when)))
                (u     (if (symbolp unless) unless (eval unless)))
                (autos (^call delete-keyword-and-arg :when autos))
                (autos (^call delete-keyword-and-arg :unless autos))
                )
           (when (and w (not u))
             (funcall ,handler sym autos))))))


  (^defn handle-import (from-ns into-ns deps)
    "Load dependencies. If dependenices is empty, load all symbols exported by FROM-NS."
    (if deps
        (loop for sym in deps do (ns/import from-ns into-ns sym))
      (loop for tpl being the hash-keys of ns/symbols-table
            and sym = (cdr tpl)
            when (string-match-p from-ns (symbol-name sym))
            do (ns/import from-ns into-ns sym))))


  (^defn autoload-dep (feature dep)
    "Autoload symbols from FEATURE, where DEP is a list of symbols to autoload."
    (let ((feat (or (when (stringp feature) feature)
                    (symbol-file feature) (symbol-name feature))))
      (if (symbolp dep)
          (autoload dep feat)
        (destructuring-bind (symbol &key docstring interactive type
                                    &allow-other-keys)
            dep
          (autoload symbol feat docstring interactive type)))))


  (^defn join-dirs (&rest directories)
    (reduce (lambda (l r) (file-name-as-directory (concat l r)))
            directories))

  (^defn ns->file (base ns)
    "Return a relative filepath for a given namespace."
    (let* ((xs   (split-string (symbol-name ns) "[.]"))
           (path (apply (^sym join-dirs) base xs))
           (path (substring path 0 -1)))
      (when xs
        (concat path ".el"))))


  (^defn handle-use (base feature autoloads)
    "Autoload the specified symbols from namespace or feature FEATURE."
    (let* ((path (^call ns->file base feature))
           (file (when (file-exists-p path) path)))
      (cond
       (autoloads
        (loop for dep in autoloads collect (^call autoload-dep (or file feature) dep)))
       (file
        (load file))
       (t
        (require feature)))))


  (^defn handle-pkg (pkg autoloads)
    "Require a package from an online repository, downloading it if needed."
    (unless (package-installed-p pkg)
      (package-install pkg))
    (if autoloads
        (loop for dep in autoloads collect (^call autoload-dep pkg dep))
      (require pkg)))


  ;; Local Variables:
  ;; byte-compile-warnings: (not cl-functions)
  ;; End:
 )
