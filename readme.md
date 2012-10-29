# Elisp Namespaces

A straight-forward implementation of namespaces for Emacs LISP.

Helps you keep the global namespace clean and protect your functions from prying REPLs.

Requires Emacs 24 or later.

## Installation

Put namespaces.el in your loadpath, then require it in your init.el:
```lisp
(require 'namespaces)
```

### Optional Steps:

1. Configure your package repositories, eg:
```lisp
;; Initialize packages.
(loop for x in '(
                 ("melpa"     . "http://melpa.milkbox.net/packages/")
                 ("marmalade" . "http://marmalade-repo.org/packages/")
                 ; ... etc
                 )
      do (add-to-list 'package-archives x)

      ;; Ensure the package archives have been initialized.
      finally (package-initialize)
      finally (unless package-archive-contents
                (package-refresh-contents)))
```

2. If needed, Set *ns-base-path* to the directory containing your elisp files. The default is `~/.emacs.d/elisp/`:
```lisp
(setq *ns-base-path* "~/.emacs.d/lisp/")
```

## Basic Usage:

### Define namespaces and members

You can define a namespace or reopen and existing one using the `namespace` macro:
```lisp
(namespace enterprise)
```

You can define namespaced functions using `defn`.
```lisp
(defn greet (name) (concat "Hello, " name "!"))
```
You apply those functions using `@call`:
```lisp
(@call greet "Spock")    ; => "Hello, Spock!"
```

Namespaced variables are defined using `def`:
```lisp
(def captain "Kirk")
```
You can retrieve the value using the `@` macro...
```lisp
(@ captain)    ; => "Kirk"
 ```
...and rebind it using `@set`:
```lisp
(@set captain "Picard")
```

Symbols defined with `defn` and `def` are private unless you explicitly export them.

### Exporting and importing namespace members

To make a namespaced symbol publicly-accessible, add it to the exports list for that namespace:
```lisp
(namespace enterprise :export [ captain ])
```
Other namespaces can now access that exported symbol by using direct qualification...
```lisp
(namespace j25)
(@ enterprise/captain) ; => "Picard"
```
Or by directly importing that symbol or namespace:
```lisp
(namespace borg :import [ enterprise ])
(@call assimilate (@ captain))
```

## More Detail

### DEFN and @CALL:

`defn` defines a function in the current namespace. It is equivalent to `defun*`.
```lisp
(defn greet (name) (concat "Hello " name "!"))
```
To apply a function declared with `defn`, use the @call macro.
```lisp
(@call greet "Spock")    ; => "Hello, Spock!"
```
The function you are calling must be accessible from the current namespace.

## DEF, @ and @SET:

`def` defines a variable in the current namespace. It is equivalent to `defvar`.
```lisp
(def captain "Kirk")
```
To retrieve the value of a variable declared with `def`, use the `@` macro:
```lisp
(@ captain)               ; => "Kirk"
```
To rebind a value, use `@set`:
```lisp
(@set captain "Picard")
```

### NAMESPACE

The `namespace` macro is used to import and export namespace symbols, load emacs
features and download elisp packages. It takes a number of keyword arguments,
which expect vectors as their values.

#### export

Make the given functions or variables externally-accessible ('public').
```lisp
(namespace foo :export [ foo-fn foo-var ])
```

#### import

Import public symbols from another namespace:
```lisp
(namespace foo :export [x])
(def x "Hello")
(namespace bar :import [foo])
(@ x)                            ; => "Hello"
```

The default behaviour is to import all public symbols. You can load a subset
by providing a list of symbols instead:
```lisp
(namespace baz :import [ (foo x y) ])
```
The example above will import only `x` and `y` from namespace `foo`.

#### use

Load another elisp file from disk or require an emacs feature. Periods (`.`)
are interpereted as path delimiters.
The *ns-base-path* variable is used to set the base of the namespace search path.
For example, the following form will attempt to load BASE/bar/baz.el:
```lisp
(namespace foo :use [ bar.baz paredit color-theme ])
```

#### packages

Download the specified elisp packages, then require or autoload them.
```lisp
(namespace foo :packages [ auto-complete ])
```

## Emacs Interop

The `defn` and `def` macros obfuscate their true symbols to prevent callers
from casually accessing private members of a namespace. You can obtain a
symbol's underlying name using the `@sym` macro. This is allows you to
interoperate with foreign elisp. For example:
```lisp
(defn private () (message "TOP SECRET"))
(defvar example-hook)
(add-hook 'example-hook (@sym private))
(run-hooks 'example-hook)                 ; check your *Messages* buffer!
```

You can also use the `@lambda` macro when you want to capture the declaring
namespace in your hooks or exported functions:
```lisp
(namespace foo :export [ x ])
(def x (@lambda () *ns*))                 ; *ns* represents the current namespace.
(namespace bar :import [ foo ])
(funcall (@ x))                           ; => foo
```
