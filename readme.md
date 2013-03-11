# Elisp Namespaces [![Build Status](https://travis-ci.org/chrisbarrett/elisp-namespaces.png)](https://travis-ci.org/chrisbarrett/elisp-namespaces)

An implementation of namespaces for Emacs Lisp. Helps you keep the
global namespace clean and protect your symbols from clobbering.

Pull requests welcome.

Requires Emacs 24 or later.

## Quick Tour

```lisp
;; Open a namespace:
(namespace agent
  ;; Require and autoload some elisp features:
  :use
  [ cl
    gnus
    (agent-mode   turn-on-agent-mode)
    (spy-training plant-explosives daring-escape) ])

;; Create a private variable:
(def real-name "Bond, James Bond")

;; Get the variable's value:
(@ real-name)
; => "Bond, James Bond"

;; Create a variable you can change:
(defmutable cover)

;; Update a mutable variable:
(@set cover "David Somerset")

;; Define a private function:
(defn identify ()
  (concat "Hello, I'm " (@ cover) "."))

;; Call a private function:
(_ identify)
; => "Hello, I'm David Somerset"

;; Quote a private symbol:
(add-hook 'border-crossed-hook (~ identify))

;; Make some values public:
(namespace agent
  :export [cover identify])

;; --------------------------------------------------

;; Open another namespace:
(namespace guards)

;; You can call an exported function directly...
(agent/identify)

;; ...or import that namespace and call it with `_`:
(namespace guards :import [agent])

(_ identify)
; => "Hello, I'm David Somerset"

;; Public vars are available only through accessor functions.
(@ agent/cover)
; => ERROR
(agent/cover)
; => "David Somerset"

;; Private symbols are kept private:
(@ agent/real-name)
; => ERROR
(agent/real-name)
; => ERROR

```

## Installation

The best way is to download and install the `namespaces` package from
melpa.

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

The `namespace` macro makes it easy to load dependencies using the
package management features in Emacs 24+; make sure you set up all that
package management stuff before you call those features.

## Usage

### Defining Namespaces and Members

You can define a namespace or reopen an existing one using the
`namespace` macro:

```lisp
(namespace enterprise)
```

You can define namespaced functions using `defn`, and apply them using
`_`.

```lisp
(defn greet (name)
  (concat "Hello, " name "!"))

(_ greet "Spock")    ; => "Hello, Spock!"
```

Namespaced vars are defined using `def`. You can retrieve their values
using the `@` macro.

```lisp
(def captain "Kirk")

(@ captain)               ; => "Kirk"
```

Vars defined using `def` are immutable. If you need a var that can
change, use `defmutable` instead.  You can change the value of a mutable
var using `@set`.

```lisp
(defmutable captain "Kirk")

(@set captain "Picard")
```

Symbols defined with `defn`, `def` and `defmutable` are private unless
you explicitly export them.  Vars defined with `defmutable` cannot be
changed with `@set` outside the defining namespace.

### Exporting and Importing Namespaced Symbols

To make a namespaced symbol publicly-accessible, add it to the exports
list for that namespace:

```lisp
(namespace enterprise :export [ captain ])
```

This makes `enterprise/captain` a public var, and generates an accessor
function.  Other namespaces can now access that symbol by invoking the
accessor, or by adding it to their namespace imports and calling using
`_`:

```lisp
(namespace federation)
(enterprise/captain)                   ; => "Picard"

(namespace borg :import [ enterprise ])
(_ captain)                        ; => "Picard"
```

These accessor functions add a nice layer of safety: if your
requirements change down the track (eg, you need logging), you can
safely reimplement an acccessor by hand without changing the interface
and breaking client code.

## Emacs Interop

Clients of your namespaced code do not need to know anything about
namespaces or the macros defined in this package.

The `namespace` macro declares the given namespace as an Emacs feature,
as if you called `(provide 'foo)`.  Clients can use the standard
`require` and `autoload` mechanisms to access your exported functions.

### Functions and Vars

Exported functions can be called using their fully qualified name:

```lisp
(namespace foo :export [greet])
(defn greet () "Hello!")

(namespace bar)
(foo/greet)      ; => "Hello!"
```

Similarly, exported vars can be read using their accessor functions:

```lisp
(namespace foo :export [x])
(def x 'hello)

(namespace bar)
(foo/x)      ; => hello
```

By design, clients cannot modify exported vars with `@set`, even if they
are defined with `defmutable`.  Package writers should probably use
`defcustom` when they want to define a var that can be customized by
clients.

The `defn`, `def` and `defmutable` macros mangle their true symbols to
ensure namespaced identifiers are unique. You can obtain the underlying
symbol using the `~` macro.  This is allows your private members to
interoperate with foreign elisp. For example:

```lisp
(defn private () (message "TOP SECRET"))

(defvar example-hook)
(add-hook 'example-hook (~ private))

(run-hooks 'example-hook)                 ; check your *Messages* buffer
```

### Closures

You can use the `lambda-` macro when you want to capture the declaring
namespace in your hooks or exported functions:

```lisp
(namespace foo :export [ x ])

;; Capture a private var in a closure.
(def private 'foo-private)
(def x (lambda- () (@ private)))

(namespace bar :import [ foo ])
(funcall (@ x))                           ; => foo-private
```

### Escape Hatches

It is sometimes necessary to enter a namespace manually within non-
namespaced code. The `in-ns` macro is provided for this purpose.

```lisp
(namespace foo)
(def x "inside foo")

(defun non-namespaced-fn ()
  (in-ns foo
    (message (@ x))))   ; => "inside foo"
```

The `in-ns` macro provides a controlled means of breaking encapsulation.
When all else fails, you can use it to access private vars or redefine a
`def` as `defmutable` in code you do not control.

```lisp
(namespace closed)
(def x "immutable")

(namespace user)
(in-ns closed
  (defmutable x)
  (@set x "now mutable!"))
```

## Namespace Macro

### Keyword Arguments

`namespace` takes a number of keyword arguments, which expect vectors as
their values.

#### :export

Make the given functions or variables externally-accessible ('public').

```lisp
(namespace foo :export [ x y z ... ])
```

#### :import

Import public symbols from another namespace:

```lisp
(namespace foo :export [x])
(def x "Hello")

(namespace bar :import [foo])
(_ x)                            ; => "Hello"
```

The default behaviour is to import all public symbols. You can load a
subset by providing a list of symbols instead:

```lisp
(namespace baz :import [ (foo x y) ])
```

The example above will import only `x` and `y` from namespace `foo`.

#### :use

Require an emacs feature.

```lisp
(namespace foo :use [cl])
```

Lists are interpreted as *autoload directives*, where the first item is
a feature name and the remainder are functions to be autoloaded.

```lisp
(namespace foo :use [ (paredit paredit-mode) ])
```

#### :packages

Download the specified elisp packages.

You can eagerly `require` the pacakge...

```lisp
(namespace foo :packages [ auto-complete ])
```

...or autoload a list of symbols:
```lisp
(namespace foo :packages [ (auto-complete auto-complete-mode) ])
```

### Conditional Loading

The above forms take optional `:when` or `:unless` keyword arguments to
specify loading conditions.  This can be useful for environment-specific
configuration.

```lisp
(namespace foo :use [ (color-theme :when (display-graphic-p)) ])
```

## Gotchas and Limitations

Due to internal name-mangling, you won't be able to use features like
eldoc or `describe-function` on symbols defined by `defn`, `def` and
`defmutable`. However, their fully-qualified exported forms work fine.

Elisp doesn't have reader macros, so there's not as much sugar as I'd
like.
