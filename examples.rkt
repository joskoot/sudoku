#lang racket/base

;=====================================================================================================

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

      (printline)
      (displayln "Valid board without solution,")
      (displayln "because the third column in the last three rows")
      (displayln "must contain the digits 1 2 3 and 4 but has place for three digits only.")

      (define board1
        '(1 2 • • • • • • •
          3 4 • • • • • • •
          • • • • • • • • •
          2 1 • • • • • • •
          4 3 • • • • • • •
          • • • • • • • • •
          • • • • • • • • •
          • • • • • • • • •
          • • • • • • • • •))

      (Sudoku board1)

      (printline)
      (displayln "Leaving out one of the digits produces a board with solutions.")

      (parameterize ((max-nr-of-solutions 3))
        (define board (apply vector board1))
        (for ((d (in-vector board)) (i (in-range 81)) #:when (number? (vector-ref board i)))
          (define old-d (vector-ref board i))
          (vector-set! board i 0)
          (Sudoku (vector->list board))
          (vector-set! board i old-d)))

      (printline)
      (displayln "6670903752021072936960 solutions.")

      (parameterize ((max-nr-of-solutions 5))
        (Sudoku
          '(• • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •)))

      (parameterize ((count-only #t) (max-nr-of-solutions 10000))
        (Sudoku
          '(• • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            • • • • • • • • •)))

      (printline)
      (displayln "Trivial, board already complete.")
      
      (Sudoku
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          6 4 5   9 7 8   3 1 2
          9 7 8   3 1 2   6 4 5))

      (printline)
      (displayln "8 solutions.")

      (Sudoku
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          • • •   • • •   • • •
          • • •   • • •   • • •))

      (printline)
      (displayln "4 solutions.")

      (Sudoku
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          • • •   • • •   3 • •
          • • •   • • •   • • •))

      (printline)
      (displayln "2 solutions.")

      (Sudoku
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          • • •   • • •   3 1 •
          • • •   • • •   6 • •))

      (printline)
      (displayln "1 solution.")

      (Sudoku
        '(• • •   5 2 4   • • 6
          9 3 •   • • •   • 7 •
          • • •   • • •   • • •

          • • •   • • 3   • • •
          2 8 •   • • •   • • •
          • • 1   7 • •   4 • •

          • • •   6 • •   1 • 7
          • 9 •   • • •   • 2 •
          • • 4   • 8 •   • 3 •))

      (printline)
      (displayln "1 solution.")

      (Sudoku
        '(• 3 •   • • •   • • • 
          • • •   1 • 5   • • • 
          • 9 8   • • •   • 6 •
     
          8 • •   • 6 •   • • 3 
          4 • •   • • 3   • • 1 
          7 • •   • 2 •   • • •
     
          • 6 •   • • •   2 8 • 
          • • •   4 • 9   • • 5 
          • • •   • • •   • 7 •))

      (printline)
      (displayln "1 solution.")

      (Sudoku
        '(• 3 •   • • •   • • • 
          • • •   1 • 5   • • • 
          • 9 8   • • •   • 6 • 

          8 • •   • 6 •   • • 3 
          4 • •   • • 3   • • 1 
          7 • •   • 2 •   • • • 

          • 6 •   • • •   2 8 • 
          • • •   4 • 9   • • 5 
          • • •   • • •   • 7 •))

      (printline)
      (displayln "Leaving out digit 6 in row 4, column 5 produces a board with 652 solutions.")

      (parameterize ((count-only #t))
        (Sudoku
          '(• 3 •   • • •   • • • 
            • • •   1 • 5   • • • 
            • 9 8   • • •   • 6 • 

            8 • •   • X •   • • 3 
            4 • •   • • 3   • • 1 
            7 • •   • 2 •   • • • 

            • 6 •   • • •   2 8 • 
            • • •   4 • 9   • • 5 
            • • •   • • •   • 7 •)))

      (printline)
      (displayln "6280 solutions.")

      (parameterize ((count-only #t))
        (Sudoku
          '(1 2 3 4 5 6 7 8 9
            4 5 6 7 8 9 1 2 3
            7 8 9 1 2 3 4 5 6
            2 3 1 • • • • • •
            5 6 4 • • • • • •
            8 9 7 • • • • • •
            3 1 2 • • • • • •
            6 4 5 • • • • • •
            9 7 8 • • • • • •)))

      (printline)
      (displayln "The same number of solutions when erasing some digits.")

      (parameterize ((count-only 'yes))
        (Sudoku
          '(• 2 3 • 5 6 • 8 9 
            4 • 6 7 • 9 1 • 3 
            7 8 • 1 2 • 4 5 • 
            • 3 1 • • • • • • 
            5 • 4 • • • • • •
            8 9 • • • • • • • 
            • 1 2 • • • • • • 
            6 • 5 • • • • • • 
            9 7 • • • • • • •)))

      (printline)
      (displayln "492752 solutions.")

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

      (printline)
      (displayln "535496 solutions.")

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
            • • • • • • • • •)))

      (printline)
      (displayln "22154 solutions.")

      (parameterize ((count-only 'yes))
        (Sudoku
          '(• 2 3 • 5 6 • 8 9 
            4 • 6 7 • 9 1 • 3 
            7 8 • 1 2 • 4 5 • 
            • 3 1 • • • • • • 
            5 • 4 • • • • • • 
            8 9 • • • • • • • 
            • 1 2 • • • • • • 
            6 • 5 • • • • • • 
            • • • • • • • • •)))

      (define board2
        '(8 • •   • • •   • • •
          • • 3   6 • •   • • •
          • 7 •   • 9 •   2 • •

          • 5 •   • • 7   • • •
          • • •   • 4 5   7 • •
          • • •   1 • •   • 3 •

          • • 1   • • •   • 6 8
          • • 8   5 • •   • 1 •
          • 9 •   • • •   4 • •))

      (printline)
      (displayln "1 solution.")

      (Sudoku  board2)

      (printline)
      (displayln "Omitting a digit from the previous example yields more than one solution.")

      (parameterize ((count-only #t))
        (define board (apply vector board2))
        (for ((d (in-vector board)) (i (in-range 81)) #:when (number? (vector-ref board i)))
          (define old-d (vector-ref board i))
          (vector-set! board i 0)
          (Sudoku (vector->list board))
          (vector-set! board i old-d)))

      (printline)
      (displayln "No solution. No field for digit 1 in row 3.")

      (Sudoku
        '(1 • • • • • • • •  
          • • • 1 • • • • •  
          • • • • • • • • 2  
          • • • • • • 1 • •  
          • • • • • • • • •  
          • • • • • • • • •   
          • • • • • • • • •  
          • • • • • • • 1 •  
          • • • • • • • • •))

      (printline)
      (displayln "Replacing digit 2 by digit 1 yields a board with many solutions.")

      (parameterize ((max-nr-of-solutions 5))
        (Sudoku
          '(1 • • • • • • • •  
            • • • 1 • • • • •  
            • • • • • • • • 1  
            • • • • • • 1 • •  
            • • • • • • • • •  
            • • • • • • • • •   
            • • • • • • • • •  
            • • • • • • • 1 •  
            • • • • • • • • •)))

      ; The following example would count all complete Sudoku boards.
      ; Do not uncomment it, for it would last too long, although it would run in constant space.
      #;
      (parameterize ((count-only #t))
        (Sudoku ; 6670903752021072936960 solutions:
          '(• • •  • • •  • • •
            • • •  • • •  • • •
            • • •  • • •  • • •
            • • •  • • •  • • •
            • • •  • • •  • • •
            • • •  • • •  • • •
            • • •  • • •  • • •
            • • •  • • •  • • •
            • • •  • • •  • • •))))
    '()))

(printline)
(displayln "End of all examples")
(printf "Nr of sudoku calls: ~s, cpu: ~s ms, real: ~s ms~n" calls cpu real)
(printf "Total nr of computed solutions: ~s~n" sols)
(printf "Mean cpu time per solution: about ~a ms~n" (~r #:precision '(= 3) (/ cpu sols)))
(printf "Mean real time per solution: about ~a ms~n~n" (~r #:precision '(= 3) (/ real sols)))

;=====================================================================================================



