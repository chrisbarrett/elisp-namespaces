;; NS-OPERATORS
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
;; Namespace operator macros.
;;

(provide 'ns-operators)
(require 'ns-core)
(eval-when-compile (require 'cl))
(setq lexical-binding t)

(defvar ns/current-ns 'ns)


;;; ------------------------------- Utilities ----------------------------------

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


;;; ------------------------------- Operators ----------------------------------

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


(defmacro* @set (symbol value)
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



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
