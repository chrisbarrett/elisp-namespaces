# Elisp Namespaces

![Namespaces in Elisp Screenshot](http://cloud.github.com/downloads/chrisbarrett/elisp-namespaces/namespaces-screenshot.png)

A straight-forward implementation of namespaces for Emacs LISP.
Helps you keep the global namespace clean and protect your functions from prying REPLs.

Requires Emacs 24 or later.

### Why Does Emacs Need Namespaces?

Elisp doesn't have namespaces, so package authors tend to use prefixes to separate their identifiers and avoid clobbering.
For instance, if you have yasnippet installed you can run `M-x describe-function yas<TAB>` to see every function defined by that feature.
This level of exposure makes it hard to distinguish the intended interface from volatile internal details.

At a deeper level, a single global namespace makes elisp programming more annoying than it needs to be. I *agonize* over what to name my utility functions, because
any name I choose could get clobbered at runtime. And prefixing everything by hand is so *manual*.

This `namespace` package provides that basic level of encapsulation using a couple of simple macros, as well as conveniences to make setting up dependencies a snap.
Elisp doesn't have reader macros, so there's not as much sugar as I'd like. You'll learn to love the `@` symbol!

## Installation

Put namespaces.el in your loadpath, then require it in your init.el:
```lisp
(require 'namespaces)
```

### Optional Configuration

1. Configure your package repositories, eg:

   ```lisp
   ;; Initialize packages.
   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
   (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))

   ;; Ensure the package archives have been initialized.
   (package-initialize)
   (unless package-archive-contents
     (package-refresh-contents))
   ```

   The `namespace` macro makes it easy to load dependencies using the package management features in Emacs 24+;
   make sure you set up all that package management stuff before you call those features.


2. If needed, set `*ns-base-path*` to the directory containing your elisp files. The default is `~/.emacs.d/elisp/`:

   ```lisp
   (setq *ns-base-path* "~/.emacs.d/lisp/")
   ```

## Basic Usage

### Defining Namespaces and Members

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

Namespaced vars are defined using `def`. You can retrieve their values using the `@` macro.
```lisp
(def captain "Kirk")

(@ captain)               ; => "Kirk"
```

Vars defined using `def` are immutable. If you need a var that can change, use `defmutable` instead.
You can change the value of a mutable var using `@set`.

```lisp
(defmutable captain "Kirk")

(@set captain "Picard")
```

Symbols defined with `defn`, `def` and `defmutable` are private unless you explicitly export them.

### Exporting and Importing Namespaced Symbols

To make a namespaced symbol publicly-accessible, add it to the exports list for that namespace:
```lisp
(namespace enterprise :export [ captain ])
```
Other namespaces can now access that symbol using direct qualification, or by adding it to their namespace imports.
```lisp
(namespace j25)
(@ enterprise/captain)                   ; => "Picard"

(namespace borg :import [ enterprise ])
(@ captain)                              ; => "Picard"
```

## De Res Macronis Nomenspationem

The `namespace` macro is a versatile beast. It is used to import and export namespace symbols, load emacs
features and download elisp packages.

### Keyword Arguments

`namespace` takes a number of keyword arguments, which expect vectors as their values.

#### :export

Make the given functions or variables externally-accessible ('public').
```lisp
(namespace foo
  :export [ x y z ... ])
```

#### :import

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

#### :use

Load another elisp file from disk or require an emacs feature. Periods (`.`)
are interpereted as path delimiters.
The `*ns-base-path*` variable is used to set the base of the namespace search path.

This example will attempt to load BASE/bar/baz.el, as well as a few emacs features.
```lisp
(namespace foo
   :use
   [ bar.baz
     paredit
     color-theme ])
```

#### :packages

Download the specified elisp packages, then require or autoload them.
```lisp
(namespace foo
  :packages [ auto-complete ])
```

### Dependency Forms and Autoloading

In addition to loading elisp features, the `:packages` and `:use` arguments allow you to autoload symbols:
```lisp
 (namespace clojure-conf
   :packages
   [ paredit
     (nrepl nrepl-mode)
     (clojure-mode (clojure-mode :interactive t)) ])
 ```
 In this example:
   1. paredit is *required*, meaning it is eagerly loaded
   2. `nrepl-mode` is *autoloaded* from the nrepl package, meaning it will be lazily loaded
   3. `clojure-mode` is also autoloaded, with a nested form allowing you to pass additional arguments to the underlying call to `autoload`.

See the Emacs documentation for `autoload` for more info.


### Conditional Loading

The above forms take optional :when or :unless keyword arguments to specify loading conditions.
This can be useful if you juggle OSes, terminals and window-systems.
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
