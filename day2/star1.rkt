#lang racket

(require threading)

(define (histogram str)
  (foldl
    (lambda (chr hist)
      (hash-set hist chr
        (add1 (or
                (hash-ref hist chr #f)
                0))))
    (hash)
    (string->list str)))

(define (has-double? hist)
  (member 2 (hash-values hist)))

(define (has-triple? hist)
  (member 3 (hash-values hist)))

(define (checksum-parts ids)
  (foldl
    (match-lambda**
      [(id (list doubles triples))
       (let ([hist (histogram id)])
         (list
           (+ doubles (if (has-double? hist) 1 0))
           (+ triples (if (has-triple? hist) 1 0))))])
    '(0 0)
    ids))

(define (ids->checksum ids)
  (match-let
    ([(list doubles triples) (checksum-parts ids)])
    (* doubles triples)))

(define (checksum in)
  (~>> (in-lines in)
       (sequence->list)
       (ids->checksum)))

(define test-cases
  '((("abcdef"
      "bababc"
      "abbcde"
      "abcccd"
      "aabcdd"
      "abcdee"
      "ababab") 12)))

(define (run-test lines)
  (checksum
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
                (if (= (run-test lines) expected)
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
      (displayln (checksum in)))))

(main)
