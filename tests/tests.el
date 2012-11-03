(provide 'ns-tests '(ns-core-tests
                     ns-operators-tests))
(require 'ert)

(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "src/"))
(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "tests/"))

(defmacro check (desc &rest body)
  "Wrap ert-deftest with a simpler interface for testing namespaces."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "-" desc)) ()
     ;; Rebind tables for tests.
     (let ((ns/symbols-table (copy-hash-table ns/symbols-table))
           (ns/imports-table (copy-hash-table ns/imports-table)))
       ,@body)))


(require 'ns-core-tests)
(require 'ns-operators-tests)
