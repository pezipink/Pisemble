#lang racket/base
;; Regression check: instruction-test.rkt must still assemble, self-check and
;; produce the image it produced when this was written.
(require rackunit racket/runtime-path racket/file file/sha1)

(define-runtime-path instruction-test "instruction-test.rkt")

(define expected-sha256-prefix "6FC779CD4836497E")

(test-case "instruction-test assembles and links to the expected image"
  (define dir (make-temporary-file "pisemble-test~a" 'directory))
  (parameterize ([current-directory dir])
    (dynamic-require instruction-test #f))
  (define image (build-path dir "instruction-test.img"))
  (check-true (file-exists? image))
  (define digest
    (call-with-input-file image sha256-bytes))
  (check-equal? (string-upcase (substring (bytes->hex-string digest) 0 16)) expected-sha256-prefix)
  (delete-directory/files dir))
