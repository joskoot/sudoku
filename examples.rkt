#lang racket/base

(require "sudoku.rkt")

; Examples
; ‘•’ is used for empty fields. It is not a period.

(parameterize ((count-only #t)) ; This board has no solution:
  (sudoku
    '(• 2 3 • • • • • •
      • 7 9 • • • • • •
      • • • 9 • • 4 1 •
      • 3 2 1 4 9 8 6 •
      • 9 7 • • • • • •
      • • • • • • • • •
      • • • • • • • • •
      • • • • • • • • •
      • • • • • • • • •)))

(sudoku ; Trivial, board already complete.
  '(1 2 3   4 5 6   7 8 9
    4 5 6   7 8 9   1 2 3
    7 8 9   1 2 3   4 5 6

    2 3 1   5 6 4   8 9 7
    5 6 4   8 9 7   2 3 1
    8 9 7   2 3 1   5 6 4

    3 1 2   6 4 5   9 7 8
    6 4 5   9 7 8   3 1 2
    9 7 8   3 1 2   6 4 5))

(sudoku ; 8 solutions:
  '(1 2 3   4 5 6   7 8 9
    4 5 6   7 8 9   1 2 3
    7 8 9   1 2 3   4 5 6

    2 3 1   5 6 4   8 9 7
    5 6 4   8 9 7   2 3 1
    8 9 7   2 3 1   5 6 4

    3 1 2   6 4 5   9 7 8
    • • •   • • •   • • •
    • • •   • • •   • • •))
   
; 4 solutions:

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

(sudoku ; 2 solutions:
  '(1 2 3   4 5 6   7 8 9
    4 5 6   7 8 9   1 2 3
    7 8 9   1 2 3   4 5 6

    2 3 1   5 6 4   8 9 7
    5 6 4   8 9 7   2 3 1
    8 9 7   2 3 1   5 6 4

    3 1 2   6 4 5   9 7 8
    • • •   • • •   3 1 •
    • • •   • • •   6 • •))

(sudoku ; 1 solution:
  '(• • •   5 2 4   • • 6
    9 3 •   • • •   • 7 •
    • • •   • • •   • • •

    • • •   • • 3   • • •
    2 8 •   • • •   • • •
    • • 1   7 • •   4 • •

    • • •   6 • •   1 • 7
    • 9 •   • • •   • 2 •
    • • 4   • 8 •   • 3 •))

(sudoku ; 1 solution:
  '(5 3 •   • 7 •   • • •
    6 • •   1 9 5   • • •
    • 9 8   • • •   • 6 •

    8 • •   • 6 •   • • 3
    4 • •   8 • 3   • • 1
    7 • •   • 2 •   • • 6

    • 6 •   • • •   2 8 •
    • • •   4 1 9   • • 5
    • • •   • 8 •   • 7 9))

(sudoku ; 3 solutions:
  '(5 3 •   • 7 •   • • •
    6 • •   1 9 5   • • •
    • 9 8   • • •   • 6 •

    8 • •   • 6 •   • • 3
    4 • •   8 • 3   • • 1
    7 • •   • 2 •   • • 6

    • 6 •   • • •   2 8 •
    • • •   4 1 9   • • •
    • • •   • 8 •   • x x))

(sudoku ; 1 solution:
  '(8 • •   • • •   • • •
    • • 3   6 • •   • • •
    • 7 •   • 9 •   2 • •

    • 5 •   • • 7   • • •
    • • •   • 4 5   7 • •
    • • •   1 • •   • 3 •

    • • 1   • • •   • 6 8
    • • 8   5 • •   • 1 •
    • 9 •   • • •   4 • •))

; For the previous example the unique solution is computed rapidly,
; but it takes some seconds to check that there are no more solutions.
; Omitting digit 2 in column 7 of row 3 yields 3219 solutions.
; On my PC this takes somewhat more than a minute.
; It is somewhat complicated, but not difficult to parallelize the computation,
; but this would obscure the essentials of the algorithm in procedure solve.

(printf "The following example may take some time.~n~n")
(flush-output)

(parameterize ((count-only #t))
  (sudoku ; 3219 solutions:
    '(8 • •   • • •   • • •
      • • 3   6 • •   • • •
      • 7 •   • 9 •   x • •

      • 5 •   • • 7   • • •
      • • •   • 4 5   7 • •
      • • •   1 • •   • 3 •

      • • 1   • • •   • 6 8
      • • 8   5 • •   • 1 •
      • 9 •   • • •   4 • •)))

; The following example would count all complete sudoku boards.
; Do not uncomment it, for it would last too long, although it would run in constant space.

#;
(parameterize ((count-only #t))
  (sudoku ; 6670903752021072936960 solutions:
    '(• • •   • • •   • • •
      • • •   • • •   • • •
      • • •   • • •   • • •

      • • •   • • •   • • •
      • • •   • • •   • • •
      • • •   • • •   • • •

      • • •   • • •   • • •
      • • •   • • •   • • •
      • • •   • • •   • • •)))

;=====================================================================================================

(displayln "End of all examples.\n")
