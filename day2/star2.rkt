#lang racket

(require threading)
(require levenshtein)

(define (match? left right)
  (= 1 (list-levenshtein left right)))

(define (ids->common-letters ids)
  (let ([id-lists (map string->list ids)])
    (for*/first ([left id-lists]
                 [right id-lists]
                 #:when (match? left right))
      (list->string
        (for/list ([left-chr left]
                    [right-chr right]
                    #:when (equal? left-chr right-chr))
          left-chr)))))

(define (common-letters in)
  (~>> (in-lines in)
       (sequence->list)
       (ids->common-letters)))

(define test-cases
  '((("abcde"
      "fghij"
      "klmno"
      "pqrst"
      "fguij"
      "axcye"
      "wvxyz") "fgij")))

(define (run-test lines)
  (common-letters
    (open-input-string
      (string-join
        (stream->list lines)
        "\n"))))

(define (test-suite)
  (displayln "Running tests...")
  (for ([test-case test-cases]
        [i (in-range (length test-cases))])
    (let ([lines (first test-case)]
          [expected (last test-case)])
      (displayln
        (format "Test ~a: ~a"
                (add1 i)
                (if (equal? (run-test lines) expected)
                  "Passed"
                  "Failed")))))
  (displayln "Tests complete."))

(define (main)
  (command-line
    #:final
    [("-t" "--test") =>
      (lambda (f)
        (test-suite)
        (exit 0))
      '("Run test suite")]

    #:args (filename)
    (let ([in (open-input-file filename)])
      (displayln (common-letters in)))))

(main)
