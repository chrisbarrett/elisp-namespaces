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
;; Macros for definining namespace members.
;;

(provide 'ns-definitions)
(require 'ns-core)
(require 'ns-operators)
(eval-when-compile (require 'cl))
(setq lexical-binding t)


(defmacro ^def (symbol value &optional docstring)
  "Define SYMBOL as an immutable var in the current namespace. Otherwise identical to `defconst`."
  (assert (symbolp symbol))
  (let ((hash (car (ns/intern ns/current-ns symbol))))
    ;; Ensure this is now an immutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) nil)
    `(defconst ,hash ,value ,docstring)))


(defmacro ^defmutable (symbol &optional value docstring)
  "Define SYMBOL as a mutable var in the current namespace. Otherwise identical to `defvar`."
  (assert (symbolp symbol))
  (let ((hash (car (ns/intern ns/current-ns symbol))))
    ;; Ensure this is now a mutable var.
    (setf (ns-meta-mutable? (ns/get-symbol-meta ns/current-ns symbol)) t)
    `(defvar ,hash ,value ,docstring)))


;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
