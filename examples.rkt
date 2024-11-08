#lang racket/base

(require "sudoku.rkt")

; Examples
; ‘•’ is used for empty fields. It is not a period.

(displayln "\nThe examples may take about a minute.\n")
(flush-output)

(define n 0)
(define (Sudoku board) (set! n (add1 n)) (sudoku board))

(define-values (ignore cpu real gc)
  (time-apply
    (λ ()

      ; This board has no solution:

      (define board1
        '(• 2 3 • • • • • •
          • 7 9 • • • • • •
          • • • • • • • • •
          • 3 2 • • • • • •
          • 9 7 • • • • • •
          • • • • • • • • •
          • • • • • • • • •
          • • • • • • • • •
          • • • • • • • • •))

      (sudoku board1)

      ; Leaving out one of the digits produces a board with solutions:

      (parameterize ((one-solution-only #t))
        (define board (apply vector board1))
        (for ((d (in-vector board)) (i (in-range 81)) #:when (number? (vector-ref board i)))
          (define old-d (vector-ref board i))
          (vector-set! board i 0)
          (sudoku (vector->list board))
          (vector-set! board i old-d)))

      (parameterize ((one-solution-only #t))
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
      
      (Sudoku ; Trivial, board already complete.
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          6 4 5   9 7 8   3 1 2
          9 7 8   3 1 2   6 4 5))

      (Sudoku ; 8 solutions:
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          • • •   • • •   • • •
          • • •   • • •   • • •))

      (Sudoku ; 4 solutions:
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          • • •   • • •   3 • •
          • • •   • • •   • • •))

      (Sudoku ; 2 solutions:
        '(1 2 3   4 5 6   7 8 9
          4 5 6   7 8 9   1 2 3
          7 8 9   1 2 3   4 5 6

          2 3 1   5 6 4   8 9 7
          5 6 4   8 9 7   2 3 1
          8 9 7   2 3 1   5 6 4

          3 1 2   6 4 5   9 7 8
          • • •   • • •   3 1 •
          • • •   • • •   6 • •))

      (Sudoku ; 1 solution:
        '(• • •   5 2 4   • • 6
          9 3 •   • • •   • 7 •
          • • •   • • •   • • •

          • • •   • • 3   • • •
          2 8 •   • • •   • • •
          • • 1   7 • •   4 • •

          • • •   6 • •   1 • 7
          • 9 •   • • •   • 2 •
          • • 4   • 8 •   • 3 •))

      (Sudoku ; 1 solution:
        '(• 3 •   • • •   • • • 
          • • •   1 • 5   • • • 
          • 9 8   • • •   • 6 •
     
          8 • •   • 6 •   • • 3 
          4 • •   • • 3   • • 1 
          7 • •   • 2 •   • • •
     
          • 6 •   • • •   2 8 • 
          • • •   4 • 9   • • 5 
          • • •   • • •   • 7 •))

      (Sudoku ; 1 solution:
        '(• 3 •   • • •   • • • 
          • • •   1 • 5   • • • 
          • 9 8   • • •   • 6 • 

          8 • •   • 6 •   • • 3 
          4 • •   • • 3   • • 1 
          7 • •   • 2 •   • • • 

          • 6 •   • • •   2 8 • 
          • • •   4 • 9   • • 5 
          • • •   • • •   • 7 •))

      (parameterize ((count-only #t))
        (Sudoku ; 652 solutions:
          '(• 3 •   • • •   • • • 
            • • •   1 • 5   • • • 
            • 9 8   • • •   • 6 • 

            8 • •   • X •   • • 3 
            4 • •   • • 3   • • 1 
            7 • •   • 2 •   • • • 

            • 6 •   • • •   2 8 • 
            • • •   4 • 9   • • 5 
            • • •   • • •   • 7 •)))

      (parameterize ((count-only #t)) ; 6280 solutions:
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

      (Sudoku  board2) ; 1 solution:

      ; Omitting a digit from the previous example yields more than one solution:

      (parameterize ((count-only #t))
        (define board (apply vector board2))
        (for ((d (in-vector board)) (i (in-range 81)) #:when (number? (vector-ref board i)))
          (define old-d (vector-ref board i))
          (vector-set! board i 0)
          (Sudoku (vector->list board))
          (vector-set! board i old-d)))

      (Sudoku ; No solution:
        '(1 • • • • • • • •  
          • • • 1 • • • • •  
          • • • • • • • • 2  
          • • • • • • 1 • •  
          • • • • • • • • •  
          • • • • • • • • •   
          • • • • • • • • •  
          • • • • • • • 1 •  
          • • • • • • • • •))

      ; The following example would count all complete Sudoku boards.
      ; Do not uncomment it, for it would last too long, although it would run in constant space.
      #;
      (parameterize ((count-only #t))
        (Sudoku ; 6670903752021072936960 solutions:
          '(• • •   • • •   • • •
            • • •   • • •   • • •
            • • •   • • •   • • •

            • • •   • • •   • • •
            • • •   • • •   • • •
            • • •   • • •   • • •

            • • •   • • •   • • •
            • • •   • • •   • • •
            • • •   • • •   • • •))))
    '()))

(displayln "\nEnd of all examples")
(printf "Nr of boards ~s, cpu ~s ms, real ~s ms~n" n cpu real)
(printf "Mean cpu time per board ~s ms~n~n" (round (/ cpu n)))

;=====================================================================================================

