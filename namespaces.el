;;; namespaces.el --- namespaces for Elisp
;;
;; Author: Chris Barrett
;; URL: https://github.com/chrisbarrett/elisp-namespaces
;; Version: 1.0.0
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

(defvar *ns-base-path* (concat user-emacs-directory "elisp/")
  "Defines the base directory for namespace resolution.")

;;;###autoload
(progn

;;; -------------------------- Load Subfeatures --------------------------------

  (require 'ns-core        "src/ns-core.el")
  (require 'ns-operators   "src/ns-operators.el")
  (require 'ns-definitions "src/ns-definitions.el")
  (require 'ns-namespace   "src/ns-namespace.el")
  (require 'ns-font-lock   "src/ns-font-lock.el")

;;; ----------------------------------------------------------------------------

  (namespace user))

(provide 'namespaces)
;;; namespaces.el ends here
