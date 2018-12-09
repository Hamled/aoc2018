#lang racket

(require threading)
(require racket/cmdline)
(require racket/set)
(require data/collection)

(define (first-repeat in)
  (define/match (fold-repeat acc n)
    [((cons freqs curr-freq) n)
     (let ([new-freq (+ curr-freq n)])
       (if (set-member? freqs new-freq)
         (raise new-freq #t)
         (cons (set-add freqs new-freq) new-freq)))])

  (with-handlers ([number? identity])
    (~>> (in-lines in)
         sequence->stream
         cycle
         (map string->number)
         (foldl fold-repeat (cons (set 0) 0)))))

(define test-cases
  '(((+1 -1) 0)
    ((+3 +3 +4 -2 -4) 10)
    ((-6 +3 +8 +5 -6) 5)
    ((+7 +7 -2 -7 -4) 14)))

(define (run-test lines)
  (first-repeat
    (open-input-string
      (string-join
        (~>> lines
             (map number->string)
             (stream->list))
        "\n"))))

(define (test-suite)
  (displayln "Running tests...")
  (for ([test-case test-cases]
        [i (in-range (length test-cases))])
    (displayln
      (let ([lines (first test-case)]
            [expected (last test-case)])
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
      (displayln (first-repeat in)))))

(main)
