#lang racket

#|====================================================================================================

A brute force recursive sudoku solver

A sudoku puzzle consists of a square board of 9 rows and 9 columns, in total 81 fields. Some of the
fields contain a digit d (<= 1 d 9). Other fields are empty. However, a row does not contain a
duplicate digit, a column neither. There are 9 disjunct subboards of 3 rows by 3 columns. A subboard
does not contain duplicate digits either. The goal of the puzzle is to fill all empty fields with a
digit while retaining the restrictions that a row, column or subboard must not contain duplicates.
Rows and columns are numbered from 1 up to and including 9.

(procedure : (sudoku . ‹lst›) --> void?
‹lst› : list of 81 elements

Each subsequent sublist of 9 elements of ‹lst› is considered to be a row. Corresponding elements of
rows are considered to be columns. All solutions are computed and printed. Argument ‹lst› is checked
to satisfy the restrictions. A field containing something else than a digit d (<= 1 d 9) is considered
to be empty. There always is at least one solution.

There are 6670903752021072936960 fully complete boards. Theoretically they can be generated and
counted by calling procedure sudoku without any digit specified, but in practice the large number
makes this impossible. I don't know of a fast method of computing this number.
See https://en.wikipedia.org/wiki/Sudoku and https://en.wikipedia.org/wiki/Mathematics_of_Sudoku.

Boards are kept in vectors. A row and column index is converted to a vector index as
(+ (* 9 (sub1 row)) (sub1 col)). Variable neighbours is a vector of 81 sets of indices, the set at
index i containing the indices of fields that must not contain the same digit as field i.

====================================================================================================|#

(define in-index (range 81))
(define in-digit (range 1 10))
(define (digit? d) (and (natural? d) (<= 1 d 9)))

(define (make-board (fill 0))
  (cond
    ((procedure? fill)
     (define board (make-board))
     (for ((index in-index)) (board-set! board index (fill index)))
     board)
    (else (make-vector 81 fill))))

(define (row/col->index row col)
  (+ (* 9 (sub1 row)) (sub1 col)))

(define board-ref
  (case-lambda
    ((board row col) (vector-ref board (row/col->index row col)))
    ((board index) (vector-ref board index))))

(define board-set!
  (case-lambda
    ((board row col value) (vector-set! board (row/col->index row col) value))
    ((board index value) (vector-set! board index value))))

(define (index->mutable-seteqv index) (mutable-seteqv))

(define neighbours
  (let ((board (make-board index->mutable-seteqv)))
    (for* ((row in-digit) (col in-digit))
      (for ((r in-digit) #:unless (= r row))
        (set-add! (board-ref board row col) (row/col->index r col)))
      (for ((c in-digit) #:unless (= c col))
        (set-add! (board-ref board row col) (row/col->index row c)))
      (let*
        ((r (add1 (* 3 (quotient (sub1 row) 3))))
         (c (add1 (* 3 (quotient (sub1 col) 3)))))
        (for* ((r (in-range r (+ r 3))) (c (in-range c (+ c 3))) #:unless (and (= r row) (= c col)))
          (set-add! (board-ref board row col) (row/col->index r c)))))
    board))

(define (board? obj) (and (list? obj) (= (length obj) 81)))

(define (read-board input)
  (unless (board? input) (raise-argument-error 'read-board "board?" input))
  (define board (make-board))
  (for ((index in-index) (d (in-list input)))
    (when (digit? d)
      (define ns (board-ref neighbours index))
      (for ((n (in-set ns)))
        (when (eqv? d (board-ref board n))
          (error 'read-board "neigbouring digit: ~s at indices ~s ~s" d index n)))
      (board-set! board index d)))
  board)

(define (print-board board)
  (for ((row in-digit))
    (for ((col in-digit))  (printf "~s " (convert-zero (board-ref board row col))))
    (newline))
  (newline))

(define (convert-zero d) (if (digit? d) d '•))

(define (solve board)
  (define nr-of-solutions 0)
  (define (solve empty)
    (let loop ((empty empty))
      (cond
        ((null? empty) (print-board board) (set! nr-of-solutions (add1 nr-of-solutions)))
        (else
          (define index (car empty))
          (unless (digit? (board-ref board index))
            (for ((d in-digit))
              (unless (yet-in-neighbour? d index board)
                (board-set! board index d)
                (loop (cdr empty))
                (board-set! board index 0))))))))
  (solve (for/list ((index in-index) #:unless (digit? (board-ref board index))) index))
  nr-of-solutions)

(define (yet-in-neighbour? d index board)
  (for/or ((i (in-set (board-ref neighbours index))))
    (= (board-ref board i) d)))

(define (sudoku . lst)
  (displayln "Starting sudoku:\nInitial board:\n")
  (define board (read-board lst))
  (print-board board)
  (displayln "Solutions:\n")
  (define-values (n cpu real gc) (time-apply solve (list board)))
  (printf "Finished:\nNr of solutions: ~s~nCPU time: about ~a seconds~n~n"
    (car n) (~r #:precision 3 (/ cpu 1000))))

;=====================================================================================================
; Examples
; Notice that ‘•’ is not a period. It is acceptable as a readable item.

(define • '•)

(sudoku
  1 2 3   4 5 6   7 8 9
  4 5 6   7 8 9   1 2 3
  7 8 9   1 2 3   4 5 6

  2 3 1   5 6 4   8 9 7
  5 6 4   8 9 7   2 3 1
  8 9 7   2 3 1   5 6 4

  3 1 2   6 4 5   9 7 8
  6 4 5   9 7 8   3 1 2
  9 7 8   3 1 2   6 4 5)

(sudoku
  1 2 3   4 5 6   7 8 9
  4 5 6   7 8 9   1 2 3
  7 8 9   1 2 3   4 5 6

  2 3 1   5 6 4   8 9 7
  5 6 4   8 9 7   2 3 1
  8 9 7   2 3 1   5 6 4

  3 1 2   6 4 5   9 7 8
  • • •   • • •   • • •
  • • •   • • •   • • •)
     
(sudoku
  1 2 3   4 5 6   7 8 9
  4 5 6   7 8 9   1 2 3
  7 8 9   1 2 3   4 5 6

  2 3 1   5 6 4   8 9 7
  5 6 4   8 9 7   2 3 1
  8 9 7   2 3 1   5 6 4

  3 1 2   6 4 5   9 7 8
  • • •   • • •   3 1 2
  • • •   • • •   • • •)
     
(sudoku
  1 2 3   4 5 6   7 8 9
  4 5 6   7 8 9   1 2 3
  7 8 9   1 2 3   4 5 6

  2 3 1   5 6 4   8 9 7
  5 6 4   8 9 7   2 3 1
  8 9 7   2 3 1   5 6 4

  3 1 2   6 4 5   9 7 8
  • • •   • • •   3 • •
  • • •   • • •   • • •)

(sudoku
  • • •   5 2 4   • • 6
  9 3 •   • • •   • 7 •
  • • •   • • •   • • •
  
  • • •   • • 3   • • •
  2 8 •   • • •   • • •
  • • 1   7 • •   4 • •
  
  • • •   6 • •   1 • 7
  • 9 •   • • •   • 2 •
  • • 4   • 8 •   • 3 •)

(sudoku
  5 3 •   • 7 •   • • •
  6 • •   1 9 5   • • •
  • 9 8   • • •   • 6 •
  
  8 • •   • 6 •   • • 3
  4 • •   8 • 3   • • 1
  7 • •   • 2 •   • • 6
  
  • 6 •   • • •   2 8 •
  • • •   4 1 9   • • 5
  • • •   • 8 •   • 7 9)

(sudoku
  5 3 •   • 7 •   • • •
  6 • •   1 9 5   • • •
  • 9 8   • • •   • 6 •
  
  8 • •   • 6 •   • • 3
  4 • •   8 • 3   • • 1
  7 • •   • 2 •   • • 6
  
  • 6 •   • • •   2 8 •
  • • •   4 1 9   • • •
  • • •   • 8 •   • • •)

(sudoku
  8 • •   • • •   • • •
  • • 3   6 • •   • • •
  • 7 •   • 9 •   2 • •
  
  • 5 •   • • 7   • • •
  • • •   • 4 5   7 • •
  • • •   1 • •   • 3 •
  
  • • 1   • • •   • 6 8
  • • 8   5 • •   • 1 •
  • 9 •   • • •   4 • •)

; The previous example rapidly computes the unique solution,
; but takes some time to check that there are no more solutions.

; The following example would list and count all complete sudoku boards.
; Do not uncomment it, for it would last too long and
; would produce more output than can be hold in memory.

#;
(sudoku
  • • •   • • •   • • •
  • • •   • • •   • • •
  • • •   • • •   • • •
  
  • • •   • • •   • • •
  • • •   • • •   • • •
  • • •   • • •   • • •
  
  • • •   • • •   • • •
  • • •   • • •   • • •
  • • •   • • •   • • •
  )

;=====================================================================================================
