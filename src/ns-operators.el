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

(defvar ns/current-ns 'ns)

(defmacro ^sym (symbol)
  "Return the hashed name of SYM."
  (assert (symbolp symbol))
  (let* ((tpl  (ns/split-sym symbol))
         (ns   (or (car tpl) ns/current-ns))
         (sym  (cdr tpl)))
    `',(ns/hash (ns/qualify ns sym))))


(defmacro ^using (ns &rest body)
  "Dynamically rebind the current namespace to NS while evaluating BODY."
  (declare (indent 1))
  (assert (symbolp ns))
  `(let ((ns/current-ns ',ns))
     ,@body))


(defun ns/resolve-hash (sym)
  "Returns the hash for the given symbol, or nil if resolution fails"
  (let* ((tpl   (ns/split-sym sym))
         (ns    (or (car tpl) ns/current-ns))
         (name  (cdr tpl)))
    (or
     ;; Resolve qualified symbol from global table,
     ;; or shadow unqualified imported symbol with local symbol.
     (ns/get-symbol-hash ns name)

     ;; Resolve unqualified symbol from imports.
     (error "Not implemented"))))


(defmacro ^ (symbol)
  "Evaluate SYMBOL as a var in the current namespace context."
  (assert (symbolp symbol))
  (let ((hash (ns/resolve-hash symbol)) )
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

  (let* ((hash (eval `(^sym ,symbol)))
         (name (__ns/qualify *ns* symbol)))

    (assert hash
            nil "Variable `%s` is undefined or inaccessible from namespace `%s`." symbol *ns*)

    (assert (__ns/accessible-p *ns* symbol)
            nil "Variable `%s` is undefined or inaccessible from namespace `%s`." symbol *ns*)

    (assert  (gethash hash __ns/mutable-syms)
            nil "Invalid use of `^set`. `%s` is immutable." name)

    `(setq ,hash ,value)))


(defmacro ^lambda (args &rest body)
  "A lambda function that captures the surrounding namespace environment."
  (declare (indent defun))
  `(lambda ,args (^using ,*ns* ,@body)))
