# SLURP 3.0.1
## What is this?
Making reader which slurp content from huge file.

## Usage

```lisp
* (let ((slurper (make-slurper (make-string-input-stream "foo") #'read-char)))
    (loop :for content := (funcall slurper)
          :while content
          :collect content))
=> (#\f #\o #\o)
```

For details, see [spec file](spec/slurp.lisp)

## Alternatives
### [Series](https://github.com/tokenrove/series) especialy `scan-file`.

#### Advantage.
* Tiny

Series is framework.
Slurp is utility.

* Well supported binary file.

`SERIES:SCAN-FILE` accepts file-name as argument.
Slurp accepts `STREAM`.

When file has metadata before its main contents, slurp is better.

#### Disadvantage
* Tiny

It may framework what you want.

## From developer

### Product's goal

### License
MIT

### Developed with
* SBCL/2.0.5

### Tested with
* SBCL/2.0.5
* CLISP/2.49
* CCL/1.12
* ECL/20.4.24

## Installation

