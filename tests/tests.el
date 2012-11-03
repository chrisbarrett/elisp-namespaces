(provide 'ns-tests '(ns-core-tests))

(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "src/"))
(add-to-list 'load-path (concat user-emacs-directory "elisp-namespaces/" "tests/"))

(require 'ns-core-tests)
