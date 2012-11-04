(provide 'ns-namespace-tests)
(require 'ns-namespace)
(require 'ert)

;;; ^namespace

(check "^namespace declaration should update current namespace"
  (^namespace foo)
  (should (equal 'foo ns/current-ns)))


;;; Exported Functions

(check "can call imported public fn using unqualified symbol"
  (^namespace foo :export [ public ])
  (defn public () 'expected)
  (^namespace bar :import [ foo ])
  (should (equal 'expected (^call public))))


(check "can call public fn using qualified symbol"
  (^namespace foo :export [ public ])
  (defn public () 'expected)
  (^namespace bar)
  (should (equal 'expected (^call foo/public))))


(check "can call public fn without ^call using qualified symbol"
  (let* ((ns    (gensym))
         (fname (intern (concat (symbol-name ns) "/" "public")))
        )
    (eval `(^namespace ,ns :export [ public ]))

    (defn public () 'expected)
    (^namespace bar)
    (should (equal 'expected (funcall fname)))))


;;; Dependency Loading

(check "can require elisp features"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(^namespace foo :use [ ,feature ]))
    (should (member feature features))))


(check "can load elisp files using period-delimited path"
  (let    ((result))
    (flet (
           (file-exists-p (x) t)
           (load          (f) (setq result f))
           )
      (^namespace foo :use [ x.y.z ])
      (should (string-match-p (concat *ns-base-path* "x/y/z.el$") result)))))


(check "can download packages"
  (let    ((pkg (gensym)) (loaded))
    (flet (
           (package-install (x) (setq loaded x))
           )
      (provide pkg)
      (eval `(^namespace foo :packages [ ,pkg ]))
      (should (eq loaded pkg)))))


(check "requires package after download"
  (flet   ((package-install (x)))
    (let  ((pkg (gensym)))
      (provide pkg)
      (eval `(^namespace foo :packages [ ,pkg ]))
      (should (member pkg features)))))


;;; Autoloading

(check "can autoload elisp features"
  (let    ((feature (gensym))
           (result))
    (flet (
           (autoload (fn file &optional doc interactive type)
             (setq result `[,fn ,file ,interactive ,type]))
           )
      (provide feature)
      (eval `(^namespace foo :use [ (,feature (x :interactive t :type boolean)) ]))
      (should (equal result `[x ,(symbol-name feature) t boolean])))))


(check "can autoload packages"
  (let    ((pkg (gensym))
           (result))
    (flet (
           (package-install (x))

           (autoload (fn file &optional doc interactive type)
             (setq result `[,fn ,file ,interactive ,type]))
           )
      (provide pkg)
      (eval `(^namespace foo :packages [ (,pkg (x :interactive t :type boolean))]))
      (should (equal result `[x ,(symbol-name pkg) t boolean])))))


;;; Conditional Loading

(check "loads a dependency when `when` evaluates to true"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(^namespace foo :use [ (,feature :when (eq t t)) ]))
    (should (member feature features))))


(check "loads a dependency when `unless` evaluates to nil"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(^namespace foo :use [ (,feature :unless (eq t nil)) ]))
    (should (member feature features))))


(check "loads a dependency when `when` is t and `unless` is nil"
  (let ((feature (gensym)))
    (provide feature)
    (eval `(^namespace foo :use [ (,feature :when t :unless nil) ]))
    (should (member feature features))))


(check "does not load a dependency when `when` evaluates to nil"
  (^namespace foo :use [ (undefined :when (eq t nil)) ]))

(check "does not load a dependency when `unless` evaluates to true"
  (^namespace foo :use [ (undefined :unless (eq t t)) ]))

(check "does not load a dependency when `when` is nil and `unless` is nil"
  (^namespace foo :use [ (undefined :when nil :unless nil) ]))

(check "does not load a dependency when `when` is t and `unless` is t"
  (^namespace foo :use [ (undefined :when t :unless t) ]))


;; Local Variables:
;; no-byte-compile: t
;; End:
