;; NS-CORE
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
;; Core namespace functions.
;;

(namespace ns
  :use [ ns-operators
         ns-definitions ])

(provide 'ns-font-lock)

(def match-kw
     (rx "(" (group
              (or (and "def" (* (any graphic)))
                  "namespace" "@using" "@lambda"))))

(defn match-identifier-after (&rest strings)
  (rx-to-string `(and "("
                      (or ,@strings) (+ space)
                      (group (+ (not (any space "()[]")))))))

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
