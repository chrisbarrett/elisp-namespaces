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

You can define a namespace or reopen an existing one using the `namespace` macro:
```lisp
(namespace enterprise)
```

You can define namespaced functions using `defn`, and apply them using `@call`.
```lisp
(defn greet (name)
  (concat "Hello, " name "!"))

(@call greet "Spock")    ; => "Hello, Spock!"
```

Namespaced variables are defined using `def`. You can retrieve their values using the `@` macro, and set them using `@set`.
```lisp
(def captain "Kirk")

(@ captain)               ; => "Kirk"

(@set captain "Picard")
```

Symbols defined with `defn` and `def` are private unless you explicitly export them.

### Exporting and importing namespace members

To make a namespaced symbol publicly-accessible, add it to the exports list for that namespace:
```lisp
(namespace enterprise :export [ captain ])
```
Other namespaces can now access that exported symbol by using direct qualification, or by adding it to their namespace imports.
```lisp
(namespace j25)
(@ enterprise/captain)                   ; => "Picard"

(namespace borg :import [ enterprise ])
(@ captain)                              ; => "Picard"
```

### De Res Macronis Nomenspationem

The `namespace` macro is a versatile beat. It is used to import and export namespace symbols, load emacs
features and download elisp packages. It takes a number of keyword arguments, which expect vectors as their values.

#### export

Make the given functions or variables externally-accessible ('public').
```lisp
(namespace foo
  :export [ x y z ... ])
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
(namespace baz
  :import [ (foo x y) ])
```
The example above will import only `x` and `y` from namespace `foo`.

#### use

Load another elisp file from disk or require an emacs feature. Periods (`.`)
are interpereted as path delimiters.
The *ns-base-path* variable is used to set the base of the namespace search path.

This example will attempt to load BASE/bar/baz.el, as well as a few emacs features.
```lisp
(namespace foo
   :use
   [ bar.baz
     paredit
     color-theme ])
```

#### packages

Download the specified elisp packages, then require or autoload them.
```lisp
(namespace foo
  :packages [ auto-complete ])
```

#### conditional loading

The above forms take optional :when or :unless keyword arguments to specify loading conditions.
This can be useful if you juggle between OSes, terminals and window-systems.
```lisp
(namespace foo
  :use [ (color-theme :when window-system) ])
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

## Gotchas

The internal mechanisms in this package do all sorts of things that make the emacs debugger experience truly terrible.
If you want to debug your functions, consider redefining your functions using `defun` while you're testing.
