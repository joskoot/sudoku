;=====================================================================================================

; A brute force recursive sudoku solver

;=====================================================================================================

#lang racket/base

(require
  (only-in racket
    range
    natural?
    mutable-seteqv
    set-add!
    in-set
    ~r
    contract-out
    ->
    any/c
    case->))

(provide
  (contract-out
    (sudoku
      (->
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        any/c any/c any/c any/c any/c any/c any/c any/c any/c
        void?))
    (examples (-> void?))
    (count-only (case-> (-> any/c void?) (-> boolean?)))))

#|====================================================================================================

Sudoku puzzles of the following type are considered: a puzzle consists of a square board of 9 rows and
9 columns, in total 81 fields. Some of the fields contain a non zero decimal digit. The other fields
are empty. However, a row does not contain a duplicate digit, a column neither. There are 9 disjunct
convex subboards, each with 9 fields in 3 rows and 3 columns. A subboard does not contain duplicate
digits either. The goal of the puzzle is to fill all empty fields with non zero decimal digits while
retaining the restrictions that a row, column or subboard must not contain duplicates. Rows and
columns are numbered from 1 up to and including 9.

   -------------------------   Each dot stand for a digit or a mark that the field is empty
   | • • • | • • • | • • • |
   | • • • | • • • | • • • |
   | • • • | • • • | • • • |
   -------------------------
   | • • • | • • • | • • • |
   | • • • | • • • | • • • |
   | • • • | • • • | • • • |
   -------------------------
   | • • • | • • • | • • • |
   | • • • | • • • | • • • |
   | • • • | • • • | • • • |
   -------------------------

   (procedure : (sudoku . ‹lst›) --> void?
   ‹lst› : list of 81 elements

Each subsequent contiguous sublist of 9 elements of ‹lst› is considered to be a row. Corresponding
elements of rows form columns. Argument ‹lst› is checked to satisfy the restrictions. A field
containing something else than a non zero decimal digit is considered to be empty. The ‹lst› does not
necesseraly represent a well composed puzzle with one solution only. All solutions are computed and
printed.

There are 6670903752021072936960 fully complete boards. Theoretically they can be generated and
counted by calling procedure sudoku without any digit specified, but in practice the large number
makes this impossible. I don't know of a fast method of computing this number.
See https://en.wikipedia.org/wiki/Sudoku and https://en.wikipedia.org/wiki/Mathematics_of_Sudoku.

The board is kept in a vector. A row and column index is converted to a vector index as

   (+ (* 9 (sub1 row)) (sub1 column))

Empty fields are replaced by 0. Variable neighbours contains a vector of 81 sets of indices, the set
at index i containing the indices of fields that must not contain the same digit as field i. Procedure
solver does a two level loop, the outer level running along all empty fields and the inner level along
all digits still allowed in this field.

====================================================================================================|#

(define count-only (make-parameter #f (λ (x) (and x #t)) 'count-only))

(define in-index (range 81))
(define in-digit (range 1 10))
(define (digit? d) (and (natural? d) (<= 1 d 9)))

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

(define neighbours
  (let ((board (build-vector 81 (λ (index) (mutable-seteqv)))))
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
  ; (unless (board? input) (raise-argument-error 'read-board "board?" input))
  (define board (make-vector 81))
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
  (define (solve empty-fields)
    (cond
      ((null? empty-fields)
       (unless (count-only) (print-board board))
       (set! nr-of-solutions (add1 nr-of-solutions)))
      (else
        (define index (car empty-fields))
        (unless (digit? (board-ref board index))
          (for ((d in-digit))
            (unless (already-in-neighbour? d index board)
              (board-set! board index d)
              (solve (cdr empty-fields))
              (board-set! board index 0)))))))
  (solve (for/list ((index in-index) #:unless (digit? (board-ref board index))) index))
  nr-of-solutions)

(define (already-in-neighbour? d index board)
  (for/or ((i (in-set (board-ref neighbours index))))
    (= (board-ref board i) d)))

(define (sudoku . lst)
  (printf "Starting sudoku:\n~
           Initial board:\n\n")
  (define board (read-board lst))
  (print-board board)
  (unless (count-only) (displayln "Solutions:\n"))
  (define-values (n cpu real gc) (time-apply solve (list board)))
  (printf "Finished:\n~
           Nr of solutions: ~s~n~
           CPU time: about ~a seconds~n~n"
    (car n) (~r #:precision 3 (/ cpu 1000))))

;=====================================================================================================
; Examples
; Notice that ‘•’ is not a period. It is acceptable as a readable item.

(define (examples)

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

  ; For the previous example the unique solution is computed rapidly,
  ; but it takes some time to check that there are no more solutions.
  ; Omitting digit 2 in column 7 of row 3 yields 3219 solutions.
  ; On my PC this takes almost 3.5 minutes.
  ; It is not difficult to parallelize the computation,
  ; but this obscures the essentials of the algorithm in procedure solve.

  (define x 'x)

  (parameterize ((count-only #t))
    (sudoku
      8 • •   • • •   • • •
      • • 3   6 • •   • • •
      • 7 •   • 9 •   x • •
  
      • 5 •   • • 7   • • •
      • • •   • 4 5   7 • •
      • • •   1 • •   • 3 •
  
      • • 1   • • •   • 6 8
      • • 8   5 • •   • 1 •
      • 9 •   • • •   4 • •))

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
    ))

;=====================================================================================================
