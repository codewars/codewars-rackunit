#lang racket/base
(require rackunit
         rackunit/private/check-info
         racket/string)

(provide run-tests)

(define (with-tag tag value)
  (format "\n<~a::>~a\n" tag (string-replace value "\n" "<:LF:>")))
(define completed (with-tag "COMPLETEDIN" ""))
(define passed (with-tag "PASSED" "Test Passed"))


(define (show-exn:test:check e)
  (format "\n<FAILED::>Expected ~v but instead got ~v\n"
          (get-check-expected e)
          (get-check-actual e)))

(define (get-check-info-value exn name)
  (ormap (lambda (info)
           (and (eq? (check-info-name info) name)
                (check-info-value info)))
         (exn:test:check-stack exn)))

(define (get-check-expected exn)
  (pretty-info-value
   (get-check-info-value exn 'expected)))
(define (get-check-actual exn)
  (pretty-info-value
   (get-check-info-value exn 'actual)))


(define (show-exn e)
  (with-tag "ERROR" (exn-message e)))


(define (fdown suite name before after failed?)
  (before)
  (display (with-tag "DESCRIBE" name))
  failed?)

(define (fup suite name before after failed kid-failed?)
  (display completed)
  (after)
  (or failed kid-failed?))

(define (fhere case name action failed?)
  (when name (display (with-tag "IT" name)))
  (define-values (result current-failed?)
    (with-handlers ([exn:test:check?
                     (lambda (e)
                       (values (show-exn:test:check e) #t))]
                    [exn?
                     (lambda (e)
                       (values (show-exn e) #t))])
      (action)
      (values passed #f)))
  (display result)
  (when name (display completed))
  (or failed? current-failed?))


(define (run-tests test)
  (let ([test-result (foldts-test-suite fdown fup fhere #f test)])
    (when test-result (exit 1))))

