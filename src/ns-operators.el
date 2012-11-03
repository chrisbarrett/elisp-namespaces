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

(defun ns/find-public-function (ns sym)
  "Gets the hash and name of a function if it is public, else nil."
  (let* ((tuple (ns/make-key ns sym))
         (meta  (ns/get-symbol-meta ns sym)))
    (when (and meta (ns-meta-public? meta))
      tuple)))

(defun ns/find-imported-sym (unqualified-sym ns)
  "Try to find the given symbol in the imports for namespace NS.
Returns the hash and name of the sym if if it succeeds, else nil"
  (let* ((tbl     (gethash ns ns/imports-table))
         (tuples  (when tbl (hash-keys tbl)))
         (match-p (lambda (tpl)
                    (let* ((name (cdr tpl))
                           (sym  (cdr (ns/split-sym name))))
                      (equal sym unqualified-sym)))))
    (car-safe
     (when tuples
       (remove-if-not match-p tuples)))))

(defun ns/resolve (sym)
  "Returns the hash for the given symbol, or nil if resolution fails"
  (let* ((tpl   (ns/split-sym sym))
         (ns    (or (car tpl) ns/current-ns))
         (name  (cdr tpl)))
    (or
     (when (ns/get-symbol-hash ns name) (ns/make-key ns name))
     (ns/find-public-function ns name)
     (ns/find-imported-sym sym ns/current-ns))))


;;; ------------------------------- Operators ----------------------------------

(defmacro ^sym (symbol)
  "Return the hashed name of SYM."
  (assert (symbolp symbol))
  (let ((hash (car-safe (ns/resolve symbol))))
    (assert hash ()
            "Symbol `%s` is undefined or inaccessible from namespace `%s`."
            symbol ns/current-ns)
    `',hash))


(defmacro ^using (ns &rest body)
  "Dynamically rebind the current namespace to NS while evaluating BODY."
  (declare (indent 1))
  (assert (symbolp ns))
  `(let ((ns/current-ns ',ns))
     ,@body))


(defmacro ^ (symbol)
  "Evaluate SYMBOL as a var in the current namespace context."
  (assert (symbolp symbol))
  (let ((hash (car-safe (ns/resolve symbol))))
    (assert hash ()
            "Symbol `%s` is undefined or inaccessible from namespace `%s`."
            symbol ns/current-ns)
    hash))


(defun ^dynamic (symbol)
  "Evaluate a namespace-qualified symbol dynamically."
  (eval `(^ ,symbol)))


(defmacro ^call (fn &rest args)
  "Apply the given namespace-qualified function."
  (assert (symbolp fn))
  (assert (__ns/accessible-p *ns* fn) ()
          "Function `%s` is undefined or inaccessible from namespace `%s`."
          fn *ns*)
  (assert (functionp (eval `(^sym ,fn))) ()
          "`%s` is not a function. Use `^` to evaluate vars." fn)
  `(funcall `,(^sym ,fn) ,@args))


(defmacro* ^set (symbol value)
  "Set the value of a namespace-qualified symbol."
  (assert (symbolp symbol))

  (let* ((tpl  (ns/resolve symbol))
         (hash (car-safe tpl))
         (name (cdr-safe tpl)))
    (cond
     (tpl
      (let* ((tpl  (ns/split-sym name))
             (ns   (car-safe tpl))
             (sym  (cdr-safe tpl))
             (meta (ns/get-symbol-meta ns sym))
             )
        (assert (ns-meta-mutable? meta) ()
                "Invalid use of `^set`. `%s` is immutable." symbol)
        `(setq ,hash ,value)))

     ;; Could not resolve SYMBOL.
     (t (error "Variable `%s` is undefined or inaccessible from namespace `%s`."
               symbol ns/current-ns)))))


(defmacro ^lambda (args &rest body)
  "A lambda function that captures the surrounding namespace environment."
  (declare (indent defun))
  `(lambda ,args (^using ,*ns* ,@body)))



;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
