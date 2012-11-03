(provide 'ns-tests '(ns-core-tests
                     ns-operators-tests))

(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "src/"))
(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "tests/"))

(require 'ns-core-tests)
(require 'ns-operators-tests)
