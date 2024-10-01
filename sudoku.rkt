#|====================================================================================================

; A brute force recursive sudoku solver

======================================================================================================

Sudoku puzzles of the following type are considered: a puzzle consists of a square board of 9 rows and
9 columns, in total 81 fields. Some of the fields contain a non zero decimal digit. The other fields
are empty. However, a row does not contain a duplicate digit, a column neither. There are 9 disjunct
convex subboards, each with 9 fields in 3 adjacent rows and 3 adjacent columns. See the figure below.
A subboard does not contain duplicate digits either. The goal of the puzzle is to fill all empty
fields with non zero decimal digits while retaining the restrictions that a row, column or subboard
must not contain duplicates. Rows and columns are numbered from 1 up to and including 9.

       1 2 3   4 5 6   7 8 9
     -------------------------   Each dot stand for a digit or a mark that the field is empty
   1 | • • • | • • • | • • • |
   2 | • • • | • • • | • • • |
   3 | • • • | • • • | • • • |
   -------------------------
   4 | • • • | • • • | • • • |
   5 | • • • | • • • | • • • |
   6 | • • • | • • • | • • • |
   -------------------------
   7 | • • • | • • • | • • • |
   8 | • • • | • • • | • • • |
   9 | • • • | • • • | • • • |
     -------------------------

======================================================================================================

procedure : (sudoku ‹lst›) --> void?
‹lst› : list of 81 elements

Each subsequent contiguous sublist of 9 elements of ‹lst› is considered to be a row. Corresponding
elements of rows form columns. Argument ‹lst› is checked to satisfy the restrictions. A field
containing something else than a non zero decimal digit is considered to be empty. The ‹lst› does not
necesseraly represent a well composed puzzle with one solution only. All solutions are computed.
If parameter count-only is #f the solutions are printed.

If parameter count-only is true or the current output port is not connected to memory, the procedure
runs in constant space. However, with parameter count-only set to #f and the current output port
connected to a file, the file may become very long or even exceed the available space of the device.

======================================================================================================

parameter : (count-only) --> boolean?
            (count-only ‹yes/no›) --> void?
‹yes/no› : any/c

If this parameter is true, solutions are not printed.
Argument ‹yes/no› is coerced to a boolean: (and ‹yes/no› #t).

======================================================================================================

procedure : (run-examples) --> void?

Runs some examples (may take some minutes).

======================================================================================================

There are 6670903752021072936960 fully complete boards. Theoretically they can be generated and
counted by calling procedure sudoku without any digit specified, but in practice the large number
makes this impossible. Counting the solutions at a rate of 1e9 per second, would last more than two
centories. I don't know of a fast method of computing this number. 
See https://en.wikipedia.org/wiki/Sudoku and https://en.wikipedia.org/wiki/Mathematics_of_Sudoku.

======================================================================================================

The board is kept in a mutable vector. A row and column index is converted to a vector index as:

   (+ (* 9 (sub1 row)) (sub1 column))

Empty fields are marked with 0. Variable neighbours contains a vector of 81 sets of indices, the set
at index i containing the indices of fields that must not contain the same digit as field i. Procedure
solver does a two level loop, the outer level running along all empty fields and the inner level along
all digits still allowed in this field. Each set has 20 elements, 8 in the row, 8 in the column and
4 additional ones in the subboard.

====================================================================================================|#

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
    any/c
    and/c
    ->
    case->))

(provide
  (contract-out
    (sudoku (-> (and/c list? (λ (lst) (= (length lst) 81))) void?))
    (run-examples (-> void?))
    (count-only (case-> (-> any/c void?) (-> boolean?)))))

(define count-only (make-parameter #f (λ (x) (and x #t)) 'count-only))
(define in-index (range 81))
(define in-digit (range 1 10))
(define (digit? d) (and (natural? d) (<= 1 d 9)))
(define (row/col->index row col) (+ (* 9 (sub1 row)) (sub1 col)))
(define (convert-zero d) (if (digit? d) d '•))
(define board "To be initialized by procedure fill-board")

(define board-ref
  (case-lambda
    ((row col) (vector-ref board (row/col->index row col)))
    ((index) (vector-ref board index))))

(define board-set!
  (case-lambda
    ((row col value) (vector-set! board (row/col->index row col) value))
    ((index value) (vector-set! board index value))))

(define (fill-board input)
  ; (unless (board? input) (raise-argument-error 'read-board "board?" input))
  (set! board (make-vector 81)) ; Initially filled with zeros.
  (for ((index in-index) (d (in-list input)))
    (when (digit? d) ; Leave empty fields zero.
      (define ns (vector-ref neighbours index))
      (for ((n (in-set ns)))
        (when (eqv? d (board-ref n))
          (error 'read-board "neigbouring digit: ~s at indices ~s ~s" d index n)))
      (board-set! index d))))

(define (print-board)
  (for ((row in-digit))
    (for ((col in-digit))  (printf "~s " (convert-zero (board-ref row col))))
    (newline))
  (newline))

; Two fields are neighbours if in the same row, the same column or the same subboard.

(define neighbours
  (let ((neighbour-vector (build-vector 81 (λ (index) (mutable-seteqv)))))
    (for* ((row in-digit) (col in-digit))
      (for ((r in-digit) #:unless (= r row))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r col)))
      (for ((c in-digit) #:unless (= c col))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index row c)))
      (let*
        ((r (add1 (* 3 (quotient (sub1 row) 3))))
         (c (add1 (* 3 (quotient (sub1 col) 3)))))
        (for* ((r (in-range r (+ r 3))) (c (in-range c (+ c 3))) #:unless (and (= r row) (= c col)))
          (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r c)))))
    neighbour-vector))

(define (already-in-neighbour? d index)
  ; Does at most 20 iterations because each field has 20 neighbours.
  (for/or ((i (in-set (vector-ref neighbours index))))
    (= (board-ref i) d)))

(define (solve)
  (define nr-of-solutions 0)
  (define (solve empty-fields)
    (cond
      ((null? empty-fields)
       (unless (count-only) (print-board))
       (set! nr-of-solutions (add1 nr-of-solutions)))
      (else
        (define index (car empty-fields))
        (unless (digit? (board-ref index))
          (for ((d in-digit))
            (unless (already-in-neighbour? d index)
              (board-set! index d)
              (solve (cdr empty-fields))
              (board-set! index 0)))))))
  (solve (for/list ((index in-index) #:unless (digit? (board-ref index))) index))
  nr-of-solutions)

(define (sudoku lst)
  (printf "Starting sudoku:\n~
           Initial board:\n\n")
  (fill-board lst)
  (print-board)
  (unless (count-only) (displayln "Solutions:\n"))
  (define-values (n cpu real gc) (time-apply solve '()))
  (printf "Finished:\n~
           Nr of solutions: ~s~n~
           CPU time: about ~a seconds~n~n"
    (car n) (~r #:precision 3 (/ cpu 1000))))

;=====================================================================================================
; Examples
; ‘•’ is used for empty fields. It is not a period.

(define (run-examples)

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

  (sudoku
    '(5 3 •   • 7 •   • • •
      6 • •   1 9 5   • • •
      • 9 8   • • •   • 6 •
  
      8 • •   • 6 •   • • 3
      4 • •   8 • 3   • • 1
      7 • •   • 2 •   • • 6
  
      • 6 •   • • •   2 8 •
      • • •   4 1 9   • • 5
      • • •   • 8 •   • 7 9))

  (sudoku
    '(5 3 •   • 7 •   • • •
      6 • •   1 9 5   • • •
      • 9 8   • • •   • 6 •
  
      8 • •   • 6 •   • • 3
      4 • •   8 • 3   • • 1
      7 • •   • 2 •   • • 6
  
      • 6 •   • • •   2 8 •
      • • •   4 1 9   • • •
      • • •   • 8 •   • • •))

  (sudoku
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
  ; but it takes some time to check that there are no more solutions.
  ; Omitting digit 2 in column 7 of row 3 yields 3219 solutions.
  ; On my PC this takes almost 3.5 minutes.
  ; It is not difficult to parallelize the computation,
  ; but this would obscure the essentials of the algorithm in procedure solve.

  (define x 'x)

  (parameterize ((count-only #t))
    (sudoku
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
    (sudoku
      '(• • •   • • •   • • •
        • • •   • • •   • • •
        • • •   • • •   • • •
  
        • • •   • • •   • • •
        • • •   • • •   • • •
        • • •   • • •   • • •
  
        • • •   • • •   • • •
        • • •   • • •   • • •
        • • •   • • •   • • •))))

;=====================================================================================================
