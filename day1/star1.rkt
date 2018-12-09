#lang racket

(require threading)
(require racket/cmdline)

(define (calc-freq in)
  (~>> (in-lines in)
       (sequence-map string->number)
       (sequence-fold + 0)))

(define (main)
  (command-line
    #:args (filename)
    (let ([in (open-input-file filename)])
      (displayln (calc-freq in)))))

(main)
