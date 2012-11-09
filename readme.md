# Elisp Namespaces

An implementation of namespaces for Emacs LISP, with an emphasis on immutabilty.
Helps you keep the global namespace clean and protect your symbols from clobbering.

Pull requests welcome.

Requires Emacs 24 or later.

## The Elevator Pitch

#### Namespace members are private unless explicitly exported (*yuss!*)
- explicit exports make your package's public interface clear
- private symbols are obfuscated

#### Declare dependencies on Elisp Features, online packages and random Elisp files in an easy and consistent way
- drastically streamlines your init.el
- centralised dependency declarations are easy to track

#### Helps you do the Right Thing with variables
- immutability is the default
- automatically generates accessor functions for exported variables
- client code can't directly access exported variables

#### It's all done with macros
- catches errors early
- no runtime penalty!

## Sample Code

```lisp
;;; Define a new namespace.

(namespace 007
  ;; Members are private, unless explicitly exported.
  :export
  [ cover greet ]
  :import
  [ spy-training ]
  ;; Download packages automatically from elpa and load
  ;; Elisp files and features.
  :packages
  [ geography
    gnus-MI6-utils ]
  :use
  [ agency.passports
    (agency.contacts.russian :when (equal (agent-location) 'Moscow)) ])

;;; Define some vars and functions in this namespace.

(def realname "Bond, James Bond.")
(defmutable cover "David Somerset")

(defn identify ()
   (concat "Hello, I'm " cover "."))

(defn update-cover ()
   "Update identity if cover is blown."
   (@set cover (spy-training/forge-passport)))


;;; Try to call in from the outside.

(namespace border-guards)
(007/cover)               ; => "David Somerset"
(007/realname)            ; => Error: Inaccessible

(007/identify)            ; => "Hello, I'm David Somerset"
(007/update-cover)        ; => Error: Inaccessible
```

You can see an example of this package in action in my init.el,
available [here](https://github.com/chrisbarrett/.emacs.d/blob/master/init.el).

### Why Does Emacs Need Namespaces?

Elisp doesn't have namespaces, so package authors tend to use prefixes to separate their
identifiers and avoid clobbering. For instance, if you have yasnippet installed you can run
`M-x describe-function yas<TAB>` to see every function defined by that feature. This level of exposure
makes it hard to distinguish the intended interface from volatile internal details.

At a deeper level, a single global namespace makes elisp programming more annoying than it needs to be.
I *agonize* over what to name my utility functions, because any name I choose could get clobbered at runtime.
And prefixing everything by hand is so *manual*.

This `namespace` package provides that basic level of encapsulation using a couple of simple macros,
as well as conveniences to make setting up dependencies a snap.

## Installation

The best way is to download and install the `namespaces` package from melpa.
1. Make sure you've configured package management in your init.el:
  ```lisp
  (require 'package)

  ;; Initialize packages.
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  ```
2. Install the `namespaces` package:
  ```lisp
  ;; Initialize namespaces.
  (unless (package-installed-p 'namespaces)
    (package-install 'namespaces))
  (require 'namespaces)
  ```

Otherwise, clone this repo and add it to your load path, then:
```lisp
(require 'namespaces)
```

### Optional Configuration

1. The `namespace` macro makes it easy to load dependencies using the package management features in Emacs 24+;
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
Vars defined with `defmutable` cannot be changed with `@set` outside the defining namespace.

### Exporting and Importing Namespaced Symbols

To make a namespaced symbol publicly-accessible, add it to the exports list for that namespace:
```lisp
(namespace enterprise :export [ captain ])
```
This makes `enterprise/captain` a public var, and generates an accessor function.
Other namespaces can now access that symbol by invoking the accessor, or by
adding it to their namespace imports and using `@call`:
```lisp
(namespace j25)
(enterprise/captain)                   ; => "Picard"

(namespace borg :import [ enterprise ])
(@call captain)                        ; => "Picard"
```
These accessor functions add a nice layer of safety: if your requirements change down the
track (eg, you need logging), you can safely reimplement an acccessor by hand without
changing the interface and breaking client code.

## Emacs Interop

Clients of your namespaced code do not need to know anything about namespaces or the `@` macros.

The `namespace` macro declares the given namespace as an Emacs feature, as if you called `(provide 'foo)`.
Clients can use the standard `require` and `autoload` mechanisms to access your exported functions.

Exported functions can be called using their fully qualified name:
```lisp
(namespace foo :export [greet])
(defn greet () "Hello!")

(namespace bar)
(foo/greet)      ; => "Hello!"
```

Similarly, exported vars can be read using auto-generated accessor functions:
```lisp
(namespace foo :export [x])
(def x 'hello)

(namespace bar)
(foo/x)      ; => hello
```

By design, clients cannot modify exported vars with `@set`, even if they are defined with `defmutable`.
Package writers should use `defcustom` when they want to define a var that can be customized by clients.

The `defn`, `def` and `defmutable` macros obfuscate their true symbols to prevent callers from casually
accessing private members of a namespace. You can obtain a symbol's underlying name using the `@sym` macro.
This is allows your private members to interoperate with foreign elisp. For example:
```lisp
(defn private () (message "TOP SECRET"))

(defvar example-hook)
(add-hook 'example-hook (@sym private))

(run-hooks 'example-hook)                 ; check your *Messages* buffer
```

You can also use the `@lambda` macro when you want to capture the declaring
namespace in your hooks or exported functions:
```lisp
(namespace foo :export [ x ])

;; Capture a private var in a closure.
(def private 'foo-private)
(def x (@lambda () (@ private)))

(namespace bar :import [ foo ])
(funcall (@ x))                           ; => foo-private
```

## De Res Macronis Nomenspationem

The `namespace` macro is a versatile beast. It is used to import and export namespace symbols,
load emacs features and download elisp packages.

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
(@call x)                            ; => "Hello"
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
   3. `clojure-mode` is also autoloaded, with a nested form allowing you to pass additional
      arguments to the underlying call to `autoload`.

See the Emacs documentation for `autoload` for more info.


### Conditional Loading

The above forms take optional `:when` or `:unless` keyword arguments to specify loading conditions.
This can be useful in your configuration if you juggle OSes or terminals and window-systems.
```lisp
(namespace foo
  :use [ (color-theme :when window-system) ])
```

## Gotchas and Limitations

Due to internal name-mangling, you won't be able to use features like eldoc or `describe-function` on symbols defined
by `defn`, `def` and `defmutable`. However, their fully-qualified exported forms work fine.

The name mangling also introduces some calling indirection that makes debugging more complicated.
If you need to do extended debugging, consider temporarily redefining your functions using `defun`.

Elisp doesn't have reader macros, so there's not as much sugar as I'd like. You'll learn to love the `@` symbol!
