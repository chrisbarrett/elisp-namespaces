(provide 'ns-tests '(ns-core-tests
                     ns-operators-tests))
(require 'ert)
(setq lexical-binding t)

(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "src/"))
(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "tests/"))


(defun clear-hash (table)
  "Delete all entries in the given hashtable."
  (assert (hash-table-p table))
  (loop for k being the hash-keys of table
        do (remhash k table))
  table)

(defmacro check (desc &rest body)
  "Wrap ert-deftest with a simpler interface for testing namespaces."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "-" desc)) ()
     ;; Rebind tables for tests.
     ;; Tables are copied to ensure custom equality comparers
     ;; are preserved.
     (let ((ns/symbols-table (clear-hash (copy-hash-table ns/symbols-table)))
           (ns/imports-table (clear-hash (copy-hash-table ns/imports-table)))
           (ns/exports-table (clear-hash (copy-hash-table ns/exports-table)))
           (ns/current-ns (gensym)))
       ,@body)))


(require 'ns-core-tests)
(require 'ns-operators-tests)
(require 'ns-definitions-tests)
(require 'ns-integration-tests)


;; Local Variables:
;; no-byte-compile: t
;; End:
