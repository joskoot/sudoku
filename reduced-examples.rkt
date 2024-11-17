#lang racket/base

(require "sudoku.rkt" (only-in racket ~r))

; Examples
; ‘•’ is used for empty fields. It is not a period.

(displayln "\nThe examples may take some minutes.\n")
(flush-output)
(define sols 0)
(define calls 0)

(define (Sudoku board)
  (set! calls (add1 calls))
  (set! sols (+ sols (sudoku board))))

(define (printline)
  (displayln "―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――\n"))

(define-values (ignore cpu real gc)
  (time-apply
    (λ ()

      (parameterize ((count-only #t))
        (Sudoku
          '(1 2 3 4 5 6 7 8 9
            4 5 6 7 8 9 1 2 3
            7 8 9 1 2 3 4 5 6
            2 3 1 • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            5 6 4 • • • • • •
            • • • • • • • • •
            • • • • • • • • •)))

      (parameterize ((count-only #t))
        (Sudoku
          '(1 2 3 4 5 6 7 8 9
            4 5 6 7 8 9 1 2 3
            7 8 9 1 2 3 4 5 6
            3 1 2 • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            5 6 4 • • • • • •
            • • • • • • • • •
            • • • • • • • • •))))

    '()))

(printline)
(displayln "End of all examples")
(printf "Nr of sudoku calls: ~s, cpu: ~s ms, real: ~s ms~n" calls cpu real)
(printf "Total nr of computed solutions: ~s~n" sols)

(printf "Mean cpu time per solution: about ~a ms~n~n"
  (~r #:precision '(= 2) (/ (round (* 100 (/ cpu sols))) 100.)))

;=====================================================================================================

