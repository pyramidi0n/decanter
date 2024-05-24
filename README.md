# Decanter

A micro web framework that gets out of your way.

Note that Decanter is alpha-quality software. It lacks a test suite and good
documentation; both are forthcoming.

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

A minimal `decanter` application:

```lisp
(require :decanter)
(use-package :decanter)

(defurls *urls*
    '("/(.*)" :regex hello))

(defapp *app* *urls*)

(defhandler hello
  (:get () "Hello, world!"))

(run *app*)
```

Stop the application:

```lisp
(stop *app*)
```

Run it in a shell script:

```lisp
(require :decanter)
(use-package :decanter)

(defurls *urls*
    '("/(.*)" :regex hello))

(defapp *app* *urls*)

(defhandler hello
  (:get () "Hello, world!"))

(run *app* :standalone t)
```

Stop the application with Ctrl+C/SIGINT.

## Documentation

Additional documentation is forthcoming.

## Links

* [Repository](https://sr.ht/~pyramidion/decanter/)

## Patches

Patches are welcome.

## License

Decanter is licensed under the two-clause BSD license.

See LICENSE.
