#|====================================================================================================

A brute force sudoku solver

======================================================================================================

Sudoku puzzles of the following type are considered: a square board of 9 rows and 9 columns, in total
81 fields. Some of the fields contain a non zero decimal digit. The other fields are empty. However,
a row does not contain a duplicate digit, a column neither. There are 9 disjunct convex subboards,
each with 9 fields in 3 adjacent rows and 3 adjacent columns. See the figure below. A subboard does
not contain duplicate digits either. The goal of the puzzle is to fill all empty fields with non zero
decimal digits while retaining the restriction that a row, column or subboard must not contain
duplicates. Rows and columns are numbered from 1 up to and including 9.

       1 2 3   4 5 6   7 8 9
     -------------------------   Each dot stand for a field, either empty or containing a non zero
   1 | • • • | • • • | • • • |   decimal digit such that rows, columns and sub boards have no
   2 | • • • | • • • | • • • |   duplicates. See file examples.rkt for examples.
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

In a completed board each row, each column and each sub board contains all digits from 1 up to and
including 9. There are 6670903752021072936960 fully complete boards. Theoretically they can be
generated and counted by calling procedure sudoku with all fields empty, but in practice the large
number makes this impossible. I don't know of a fast method of computing this number.
See https://en.wikipedia.org/wiki/Sudoku and https://en.wikipedia.org/wiki/Mathematics_of_Sudoku and
https://people.math.sc.edu/girardi/sudoku/enumerate.pdf. Given a solution, combination of the
following operations yield (3!)↑8 = 1679616 distinct correctly filled boards:

   Permutation of rows 1, 2 and 3; factor 3!;
   Permutation of rows 4, 5 and 6; factor 3!;
   Permutation of rows 7, 8 and 9; factor 3!;
   Permutation of columns 1, 2 and 3; factor 3!;
   Permutation of columns 4, 5 and 6; factor 3!;
   Permutation of columns 7, 8 and 9; factor 3!;
   Permutation of the three rows of sub boards; factor 3!;
   Permutation of the three columns of sub boards; factor 3!.

We have 6670903752021072936960 / (3!)↑8 = 3971683856322560 = (2↑12)×5×7×27704267971.
(‘↑’ indicates exponentiation).

Not all incomplete boards have a solution, For example:

   (parameterize ((count-only #t))
     (sudoku
      '(• 2 3 • • • • • •
        • 7 9 • • • • • •
        • • • 9 • • 4 1 •
        • 3 2 1 4 9 8 6 •
        • 9 7 • • • • • •
        • • • • • • • • •
        • • • • • • • • •
        • • • • • • • • •
        • • • • • • • • •))) ; Prints:
   
   Starting sudoku:
   Initial board:
   
   • 2 3 • • • • • • 
   • 7 9 • • • • • • 
   • • • 9 • • 4 1 • 
   • 3 2 1 4 9 8 6 • 
   • 9 7 • • • • • • 
   • • • • • • • • • 
   • • • • • • • • • 
   • • • • • • • • • 
   • • • • • • • • • 

   Nr of solutions: 0

======================================================================================================

Procedure : (sudoku ‹lst›) --> void?
‹lst› : list of 81 elements of arbitrary type

Each subsequent contiguous sublist of 9 elements of ‹lst› is considered to be a row. Corresponding
elements of rows form columns. Argument ‹lst› is checked to satisfy the restrictions. A field
containing something else than a non zero decimal digit is considered to be empty. The ‹lst› does not
necesseraly represent a well composed puzzle with one solution only. All solutions are computed.
The solutions are counted and if parameter count-only is #f they are printed too.

If parameter count-only is true or the current output port does not gather output in memory, the
procedure runs in constant space. However, with parameter count-only set to #f and the current output
port connected to a file, memory consumption remains bound, but the file may become very long or even
exceed the space available in the device. With count-only set to #f and output gathered in memory
the number of solutions may be too large to fit in memory. However, by definition a well composed
puzzle has one solution only.

======================================================================================================

Parameter : (count-only) --> boolean?
            (count-only ‹yes/no›) --> void?
‹yes/no› : any/c
Initial value : #f

If this parameter is true, solutions are not printed.
Argument ‹yes/no› is coerced to a boolean: (and ‹yes/no› #t).

======================================================================================================

The board is kept in a mutable vector. A row and column index is converted to a vector index as:

   (+ (* 9 (sub1 row)) (sub1 column))

Empty fields are marked with 0 and printed as ‘•’. Variable neighbours contains a vector of 81 sets
of indices, the set at index i containing the indices of fields that must not contain the same digit
as field i. Each field has 20 neighbours, 8 in its row, 8 in its column and 4 additional ones in its
subboard. Procedure solver does a two level loop, the outer loop along empty fields and the inner loop
along all digits still allowed in the currently selected empty field. The outer loop selects a field
with the least number of digits still not in a neighbour. This speeds up by reducing the number of
cycles in the inner loop, although some time is lost because during every cycle of the outer loop the
field to be selected must be looked for. Much more time is saved than lost.

====================================================================================================|#

#lang racket/base

(require
  (only-in racket
    natural?
    mutable-seteqv
    set-add!
    in-set
    contract-out
    any/c
    and/c
    ->
    case->))

(provide
  (contract-out
    (sudoku (-> (and/c list? (λ (lst) (= (length lst) 81))) void?))
    (count-only (case-> (-> any/c void?) (-> boolean?)))))

(define (sudoku lst)
  (printf "Starting sudoku:\n~
           Initial board:\n\n")
  (fill-board lst)
  (print-board)
  (unless (count-only) (displayln "Solutions:\n"))
  (printf "Nr of solutions: ~s~n~n" (solve)))

(define (solve)
  (define nr-of-solutions 0)
  (define (solve empty-fields)
    (cond
      ((null? empty-fields)
       (unless (count-only) (print-board))
       (set! nr-of-solutions (add1 nr-of-solutions)))
      (else
        (define-values (index digits) (find-least-empty-field empty-fields))
        ; Backtrack when the tried digits prohibit a solution,
        ; id est, when there is a field whose neighbours already have all 9 digits.
        (when (and index (not (null? digits)))
          (for ((d (in-list digits)))
            (board-set! index d)
            (solve (remove index empty-fields))
            (board-set! index 0))))))
  (solve (for/list ((index in-indices) #:unless (digit? (board-ref index))) index))
  nr-of-solutions)

(define count-only (make-parameter #f (λ (x) (and x #t)) 'count-only))
(define in-indices (in-range 81))
(define in-digits (in-range 1 10))
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
  (for ((index in-indices) (d (in-list input)))
    (when (digit? d) ; Leave empty fields zero.
      (define ns (vector-ref neighbours index))
      (for ((n (in-set ns)))
        (when (= d (board-ref n))
          (error 'read-board "neigbouring digit: ~s at indices ~s ~s" d index n)))
      (board-set! index d))))

(define (print-board)
  (for ((row in-digits))
    (for ((col in-digits))  (printf "~s " (convert-zero (board-ref row col))))
    (newline))
  (newline))

; Two fields are neighbours if in the same row, the same column or the same subboard. neighbours is
; a vector of 81 elements, element i being the set of the indices of all neighbours of field i.

(define neighbours
  (let ((neighbour-vector (build-vector 81 (λ (index) (mutable-seteqv)))))
    (for* ((row in-digits) (col in-digits))
      (for ((r in-digits) #:unless (= r row))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r col)))
      (for ((c in-digits) #:unless (= c col))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index row c)))
      (let*
        ((r (add1 (* 3 (quotient (sub1 row) 3))))
         (c (add1 (* 3 (quotient (sub1 col) 3)))))
        (for* ((r (in-range r (+ r 3))) (c (in-range c (+ c 3))) #:unless (and (= r row) (= c col)))
          (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r c)))))
    neighbour-vector))

(define (find-least-empty-field empty-fields)
  (find-least-field
    (for/list ((field (in-list empty-fields)))
      (cons field
        (for/list
          ((d in-digits)
           #:when (for/and ((nb (in-set (vector-ref neighbours field))))
                    (not (= (board-ref nb) d))))
          d)))))

(define (find-least-field indices/digits)
  (cond
    ((null? indices/digits) (values #f #f))
    ((for/or ((ds (in-list (map cdr indices/digits)))) (null? ds)) (values #f #f)) 
    (else
      (define f (caar indices/digits))
      (define ds (cdar indices/digits))
      (let loop ((f f) (ds ds) (index/digits (cdr indices/digits)) (n (length ds)))
        (cond
          ((null? index/digits) (values f ds))
          (else
            (define new-f (caar index/digits))
            (define new-ds (cdar index/digits))
            (define new-n (length new-ds))
            (cond
              ((= new-n 1) (values new-f new-ds))
              ((< new-n n) (loop new-f new-ds (cdr index/digits) new-n))
              (else (loop f ds (cdr index/digits) n)))))))))

(define (~r x) ; Format x/1000 in positional notation with three decimals followig the decimal period.
  (define-values (i f) (quotient/remainder x 1000))
  (format "~s.~a" i (align f)))

(define (align f) ; Add zeros to the left of the fraction such as to form 3 digits.
  (define str (format "~s" f))
  (define len (string-length str))
  (string-append (make-string (- 3 len) #\0) str))

;=====================================================================================================
