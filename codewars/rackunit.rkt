#lang racket/base
(require rackunit
         rackunit/private/check-info
         racket/list
         racket/string
         racket/match)

(provide run-tests)

;;; tags
(define (with-tag tag value)
  (format "\n<~a::>~a\n" tag (string-replace value "\n" "<:LF:>")))
(define completed (with-tag "COMPLETEDIN" ""))
(define passed (with-tag "PASSED" "Test Passed"))


;;; handle failed test output
(define (log-exn:test:check e mode)
  (with-tag "FAILED"
    (check-infos->string
     (check-info-stack-filter (exn:test:check-stack e) mode))))


;;; check-info stack filter
(define *quiet-check-infos* '(message actual excepted))
(define *simple-check-infos* '(name location message actual excepted))
(define *verbose-check-infos* '(expression params))

(define (check-info-stack-filter stack mode)
  (case mode
    [(quiet) (filter quiet-check-info? stack)]
    [(simple) (filter simple-check-info? stack)]
    [(custom) (filter not-verbose-check-info? stack)]
    [(all) stack]))

(define (quiet-check-info? info)
  (member info *quiet-check-infos* check-info-with-name?))
(define (simple-check-info? info)
  (member info *simple-check-infos* check-info-with-name?))
(define (verbose-check-info? info)
  (member info *verbose-check-infos* check-info-with-name?))
(define (not-verbose-check-info? info)
  (not (verbose-check-info? info)))

(define (check-info-with-name? info name)
  (eq? (check-info-name info) name))

;;; display check-infos
(define (check-infos->string stack)
  (define name-width (max-name-width stack))
  (define lines
    (flatten (map (lambda (info)
                    (check-info->string info name-width 0))
                  stack)))
  (string-join lines "<:LF:>"))

(define (max-name-width stack)
  (apply max (map check-info-name-width stack)))

(define (check-info-name-width check-info)
  (string-length
   (symbol->string
    (check-info-name check-info))))

(define *least-name-value-space* 2)
(define *nested-indent* 2)

(define (check-info->string info name-width indent-width)
  (define name (check-info-name info))
  (define value (check-info-value info))
  (define name-str (info-name->string name name-width indent-width))

  (match value
    [(nested-info nested)
     (cons (format "~a:" name-str)
           (map (lambda (info)
                  (check-info->string info
                                      name-width
                                      (+ indent-width *nested-indent*)))
                nested))]
    [(dynamic-info proc)
     (check-info->string (make-check-info name (proc))
                         name-width
                         indent-width)]
    [_
     (string-append name-str
                    (info-value->string value))]))

(define (info-name->string name name-width indent-width)
  (define name-str (symbol->string name))
  (define indent (make-string indent-width #\space))
  (define space (make-string
                 (+ (- name-width (string-length name-str) indent-width)
                    *least-name-value-space*)
                 #\space))
  (string-append indent name-str ":" space))


;;; handle test rasied an error
(define (log-raised v)
  (with-tag "ERROR" (raised-message v)))

(define (raised-message v)
  (if (exn? v)
      (exn-message v)
      (format "A value other than an exception was raised: ~e" v)))


;;; fdown, fup, fhere:
;;;   tarverse functions using in `foldts-test-suite`
(define (fdown suite name before after failed?)
  (display (with-tag "DESCRIBE" name))
  (before)
  failed?)

(define (fup suite name before after failed kid-failed?)
  (after)
  (display completed)
  (or failed kid-failed?))

(define ((fhere mode) case name action failed?)
  (when name (display (with-tag "IT" name)))
  (define-values (result here-failed?)
    (with-handlers ([exn:test:check?
                     (lambda (e)
                       (values (log-exn:test:check e mode) #t))]
                    [(lambda (x) #t)
                     (lambda (e)
                       (values (log-raised e) #t))])
      (action)
      (values passed #f)))
  (display result)
  (when name (display completed))
  (or failed? here-failed?))


;;; run-tests: test-suite * ('simple 'custom 'all) -> (void)
;;; #:mode keyword argument is one of 'simple, 'custom and 'all,
;;; it controls how to display the check-info stack.
;;;   'quiet mode displays message, actual, and excepted;
;;;   'simple mode displays all in 'quiet mode and name and location;
;;;   'custom mode displays all in 'simple mode and user customed check-info;
;;;   'all mode displays the whole check-info stack.
(define (run-tests test #:mode [mode 'quiet])
  (let ([test-result
         (foldts-test-suite fdown fup (fhere mode) #f test)])
    (when test-result (exit 1))))

