# codewars-rackunit

Wrapping Rackunit test result for Codewars.

This package contains a module `codewars/rackunit`, providing a function 
`run-tests`. It wraps the rackunit's check, makes the test output fit in
(a subset of) Codewars output format.

## Install

```
raco pkg install https://github.com/Codewars/codewars-rackunit.git
``` 

## Usage

`run-tests` takes 2 arguments, `test` and an optional keyword `#:mode` 
argument `mode`. In other words, it looks like `(run-tests test #:mode [mode])`.

The `test` argument is a `test-suite?` or `test-case?` value. It is the 
test run.

The `mode` argument decides how the result be outputed. It can be one of
`'quiet`, `'simple`, `'all` or be a list of symbols. Each kind of value 
corresponds to a mode of the output:

The quiet mode is the default value for `mode`. In this mode, output looks
like "Expected \*\*\*, but instead got \*\*\*". If the check has message,
the message is shown in the next line.

The other 3 mode display the result more RackUnit-like. They display
mode-specific `check-info`s line by line. The simple mode displays the
name, message, actual and expected infos. The all mode displays all the
`check-info`s.

When `mode` is a list of symbols, it is used as a filter for the names 
of `check-info` stack. You can use this mode to do some customization.

## Some Notes

`run-tests` here is just a wrapper for the result of RackUnit's tests.
Please read [RackUnit document](https://docs.racket-lang.org/rackunit/index.html)
to get more information about RackUnit and its api.

If you want to customize the result output, use [`define-check`](https://docs.racket-lang.org/rackunit/api.html#(form._((lib._rackunit%2Fmain..rkt)._define-check)))
and [`with-check-info`](https://docs.racket-lang.org/rackunit/api.html#(form._((lib._rackunit%2Fmain..rkt)._with-check-info))) 
to add custom infos; and the custom mode of `run-tests` to display the
info you need.

A wrapper **function** for custom check will break the location `check-info`
of checks. If you need location information displayed, please use 
`define-check` or a wrapper **macro**.
