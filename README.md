# Decanter

A micro web framework that gets out of your way.

For general information, check [here](https://decanter.cddr.io/).

Thorough documentation is available [here](https://decanter.cddr.io/docs/).

## Table of Contents

1. [Installation](#installation)
2. [Usage](#usage)
3. [Documentation](#documentation)
4. [Links](#links)
5. [Patches](#patches)
6. [License](#license)

## Installation

Decanter is available on [Ultralisp](https://ultralisp.org/) and is easy to
install using [Quicklisp](https://www.quicklisp.org/beta/).

Add the Ultralisp repository:

```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/")
```

Install Decanter:

```lisp
(ql:quickload :decanter)
```

## Usage

A minimal Decanter application:

```lisp
(require :decanter)
(use-package :decanter)

(app *app*
  ("/" (:get () "Hello, world!")))
```

Or:

```lisp
(require :decanter)
(use-package :decanter)

(defurls *urls*
    '("/" hello))

(defapp *app* *urls*)

(defhandler hello
  (:get () "Hello, world!"))

(run *app*)
```

Stop the application:

```lisp
(stop *app*)
```

## Documentation

Thorough documentation is available [here](https://decanter.cddr.io/docs/).

## Links

* [Repository](https://git.sr.ht/~pyramidion/decanter/)

## Patches

Patches are welcome.

## License

Decanter is licensed under the two-clause BSD license.

See LICENSE.
