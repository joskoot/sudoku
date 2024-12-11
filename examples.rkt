#lang racket/base

;=====================================================================================================

(require "sudoku.rkt" (only-in racket ~r))

(count-only 'yes)

; Examples
; ‘•’ is used for empty fields. It is not a period.

(displayln "\nThe examples may take some minutes.\n")
(flush-output)
(define sols 0)
(define calls 0)

(define (sudoku board)
  (set! calls (add1 calls))
  (collect-garbage)
  (set! sols (+ sols (Sudoku board))))

(define (printline)
  (displayln "―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――\n"))

(define-values (ignore cpu real gc)
  (time-apply
    (λ ()

      (printline)
      (displayln "Valid board without solution,")
      (displayln "because the third column in the last three rows")
      (displayln "must contain the digits 1 2 3 and 4 but has place for three of them only.")

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

      (sudoku board1)

      (printline)
      (displayln "Leaving out one of the digits produces a board with solutions.")

      (parameterize ((max-nr-of-solutions 3))
        (define board (apply vector board1))
        (for ((d (in-vector board)) (i (in-range 81)) #:when (number? (vector-ref board i)))
          (define old-d (vector-ref board i))
          (vector-set! board i 0)
          (sudoku (vector->list board))
          (vector-set! board i old-d)))

      (printline)
      (displayln "Example of a board with 5 non empty fields only and without solution.")
      (displayln "No field for digit 1 in row 3.")

      (sudoku
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
        (sudoku
          '(1 • • • • • • • •  
            • • • 1 • • • • •  
            • • • • • • • • 1  
            • • • • • • 1 • •  
            • • • • • • • • •  
            • • • • • • • • •   
            • • • • • • • • •  
            • • • • • • • 1 •  
            • • • • • • • • •)))

      (printline)
      (displayln "6670903752021072936960 solutions.")

      (parameterize ((max-nr-of-solutions 5))
        (sudoku
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
        (sudoku
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
      
      (sudoku
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

      (sudoku
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

      (sudoku
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

      (sudoku
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

      (sudoku
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

      (sudoku
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

      (sudoku
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
        (sudoku
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
        (sudoku
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
        (sudoku
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
        (sudoku
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
        (sudoku
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
      (displayln "535496 solutions, same as in the previous example.")

      (parameterize ((count-only #t))
        (sudoku
          '(1 2 3 7 8 9 5 6 4
            4 5 6 1 2 3 8 9 7
            7 8 9 4 5 6 2 3 1
            5 6 4 • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            3 1 2 • • • • • •
            • • • • • • • • •
            • • • • • • • • •)))

      (printline)
      (displayln "175336 solutions.")

      (parameterize ((count-only #t))
        (sudoku
          '(1 2 3 7 8 9 5 6 4
            4 5 6 1 2 3 8 9 7
            7 8 9 4 5 6 2 3 1
            9 6 4 • • • • • •
            • • • • • • • • •
            • • • • • • • • •
            5 7 8 • • • • • •
            • • • • • • • • •
            • • • • • • • • •)))

      (printline)
      (displayln "22154 solutions.")

      (parameterize ((count-only 'yes))
        (sudoku
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

      (sudoku  board2)

      (printline)
      (displayln "Omitting a digit from the previous example yields more than one solution.")

      (parameterize ((count-only #t))
        (define board (apply vector board2))
        (for ((d (in-vector board)) (i (in-range 81)) #:when (number? (vector-ref board i)))
          (define old-d (vector-ref board i))
          (vector-set! board i 0)
          (sudoku (vector->list board))
          (vector-set! board i old-d)))

      ; The following example would count all complete sudoku boards.
      ; Do not uncomment it, for it would last too long, although it would run in constant space.
      #;
      (parameterize ((count-only #t))
        (sudoku ; 6670903752021072936960 solutions:
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

(define (~r3 x) (~r #:precision '(= 3) x))

(printline)
(displayln "End of all examples")
(printf "Nr of Sudoku calls: ~s~n" calls)
(printf "Total nr of computed solutions: ~s~n" sols)
(printf "Total cpu  time: about ~a minutes~n" (~r3 (/ cpu 60000)))
(printf "Total real time: about ~a minutes~n" (~r3 (/ real 60000)))
(printf "Total gc   time: about ~a minutes~n" (~r3 (/ gc 60000)))
(printf "Mean  cpu  time per solution: about ~a ms~n" (~r3 (/ cpu sols)))
(printf "Mean  real time per solution: about ~a ms~n" (~r3 (/ real sols)))
(printf "Mean  gc   time per solution: about ~a ms~n~n" (~r3 (/ gc sols)))

;=====================================================================================================

