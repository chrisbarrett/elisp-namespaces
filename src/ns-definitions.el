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


;;; ------------------------------- Utilities ----------------------------------

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


;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
