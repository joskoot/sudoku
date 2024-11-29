#|====================================================================================================

A brute force Sudoku solver

======================================================================================================

A simple program solving Sudoku puzzles of the following type: a square board of 9 rows and 9 columns,
in total 81 fields. Some of the fields contain a non zero decimal digit. The other fields are empty.
However, a row does not contain duplicates, a column neither. There are 9 disjunct convex subboards,
each with 9 fields in 3 adjacent rows and 3 adjacent columns. See the figure below. A subboard does
not contain duplicates either. The goal of the puzzle is to fill all empty fields with non zero
decimal digits while retaining the restriction that a row, column or subboard must not contain
duplicates. Rows and columns are numbered from 1 up to and including 9.

       1 2 3   4 5 6   7 8 9
     -------------------------   Each dot stand for a field, either empty or containing a non zero
   1 | • • • | • • • | • • • |   decimal digit such that rows, columns and subboards have no
   2 | • • • | • • • | • • • |   duplicates.
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

In a completed board each row, each column and each subboard contains all digits from 1 up to and
including 9. There are 6670903752021072936960 fully complete boards. Theoretically they can be
generated and counted by calling procedure sudoku with all fields empty, but in practice the large
number makes this impossible. For a feasible method to compute the number, see
https://en.wikipedia.org/wiki/sudoku, https://en.wikipedia.org/wiki/Mathematics_of_Sudoku and
https://people.math.sc.edu/girardi/sudoku/enumerate.pdf.

A puzzle is well composed if it has one solution only and is minimally composed if it is well
composed and erasing one of the non empty fields produces a puzzle with more than one solution.
Not all valid boards have a solution. See file examples.rkt.

======================================================================================================

Procedure : (Sudoku ‹board›) --> natural?
‹board› : board?

A board is a list of 81 elements. Each subsequent contiguous sublist of 9 elements of ‹board› is
considered to be a row. Corresponding elements of rows form columns. Argument ‹board› is checked to
satisfy the restrictions. A field containing something else than an exact integer number n with 1≤n≤9,
is considered to be empty. The ‹board› does not necessarily represent a well composed puzzle with one
solution only. If parameter max-nr-of-solutions is #f, all solutions are computed. The solutions are
counted and if parameter count-only is #f they are printed too. Parameter max-nr-of-solutions limits
the number of solutions to be looked for. If parameter count-only is true or the current output port
does not gather output in memory, the procedure runs in constant space. With parameter count-only set
to #f and the current output port connected to a file, memory consumption remains bound, but the file
may become very long or even exceed the space available in the device. With the parameter set to #f
and output gathered in memory the number of solutions may be too large to fit in memory. However,
by definition a well composed puzzle has one solution only. Procedure Sudoku returns the number of
computed solutions.

======================================================================================================

Parameter : (count-only) --> boolean?
            (count-only ‹yes/no›) --> void?
‹yes/no› : any/c, initial value : #f

If this parameter is true, solutions are not printed.
Argument ‹yes/no› is coerced to a Boolean: (and ‹yes/no› #t).

======================================================================================================

Parameter : (max-nr-of-solutions) --> (or/c #f natural?)
            (max-nr-of-solutions ‹n›) --> void?
‹n› : (or/c #f natural?) = #f

If this parameter is not #f, at most ‹n› solution are looked for, else all solutions are looked for.

======================================================================================================

The board is kept in a mutable vector. A row number r and column number c are converted to a vector
index as follows:

   index = 9×(r-1) + (c-1)

Empty fields are replaced by 0 and printed as ‘•’. Variable neighbours contains a vector of 81 lists
of indices, the list at index i containing the indices of fields that must not contain the same digit
as field i. Each field has 20 neighbours, 8 in its row, 8 in its column and 4 additional ones in its
subboard. Procedure solver does a two level loop, the outer loop along empty fields and the inner
loop along all digits still allowed in the currently selected empty field. The outer loop selects a
field with the least number of digits still not in a neighbour. This speeds up by reducing the number
of cycles in the inner loop, although some time is lost because during every cycle of the outer loop
the field to be selected for the inner loop must be looked for. Much more time is saved than lost.

====================================================================================================|#

#lang racket/base

(require
  (only-in racket
    natural?
    seteq
    set->list
    mutable-seteq
    set-add!
    contract-out
    any/c
    and/c
    or/c
    ->
    case->
    sqr
    ~r)
  (only-in math/number-theory factorize))

(provide
  (contract-out
    (sudoku (-> board? natural?))
    (count-only Bool-parameter)
    (max-nr-of-solutions Max-parameter)
    #;(board? (-> any/c boolean?))))

(define-syntax-rule (Parameter in out) (and/c parameter? (case-> (-> in void?) (-> out))))
(define Bool-parameter (Parameter any/c boolean?))
(define Max-parameter (Parameter (or/c #f natural?) (or/c #f natural?)))

; Dimensions for 9×9 board.
; K can be adapted for a board of N rows by N columns, N=K↑2, with a total of N↑2 fields
; and subboards of N fields. Allowed are elements d, 1≤d≤N.

(define K 3)
(define N (sqr K))
(define N↑2 (sqr N))
(define nr-of-solutions 0)

(define (sudoku board)
  (fill-board board)
  (displayln "\nStarting sudoku")
  (cond
    ((count-only) (displayln "Counting solutions only"))
    (else
      (define max (max-nr-of-solutions))
      (if max
        (printf
          "Looking for ~s solution(s) only (ignoring possible more solutions)~n"
          max)
        (displayln "Looking for all solutions"))))
  (displayln "Initial board:\n")
  (print-board)
  (define-values (results cpu real gc) (time-apply solve '()))
  (displayln "Finishing sudoku")
  (cond
    ((zero? nr-of-solutions) (displayln "No solution found"))
    (else (printf "nr of solutions: ~s ~a~n" nr-of-solutions (factors nr-of-solutions))))
  (printf "cpu ~s ms, real ~s ms~n" cpu real)
  (when (>  nr-of-solutions 1)
    (printf "Mean cpu time per solution: about ~a ms~n"
      (~r #:precision '(= 3) (/ cpu nr-of-solutions))))
  (newline)
  nr-of-solutions)

(define (solve)
  (let/cc exit
    (set! nr-of-solutions 0)
    (define (solve empty-fields)
      (cond
        ((null? empty-fields)
         (unless (count-only) (print-board #t))
         (set! nr-of-solutions (add1 nr-of-solutions))
         (define max (max-nr-of-solutions))
         (when (and max (>= nr-of-solutions max)) (exit)))
        (else
          (define-values (field digits) (find-least-empty-field/digits empty-fields))
          (when field
            (for ((d (in-list digits)))
              (board-set! field d)
              (solve (remove field empty-fields))
              (board-set! field 0))))))
    (solve (for/list ((index in-indices) #:unless (digit? (board-ref index))) index))))

(define (coerce-to-boolean x) (and x #t))
(define count-only (make-parameter #f coerce-to-boolean 'parameter:count-only))
(define max-nr-of-solutions (make-parameter #f values 'parameter:max-nr-of-solutions))
(define in-indices (in-range N↑2))
(define in-digits (in-range 1 (add1 N)))
(define (digit? d) (and (natural? d) (<= 1 d N)))
(define (row/col->index row col) (+ (* N (sub1 row)) (sub1 col)))
(define (convert-zero d) (if (digit? d) d '•))
(define board "To be initialized by procedure fill-board")
(define (board? obj) (and (list? obj) (= (length obj) N↑2)))

(define (factors n)
  (cond
    ((<= n 1) "")
    (else
      (define facts (factorize n))
      (cond
        ((and (= (length facts) 1) (= (cadar facts) 1)) "prime")
        (else
          (define str-port (open-output-string))
          (parameterize ((current-output-port str-port))
            (display "= ")
            (let loop ((factors facts) (first? #t))
              (unless (null? factors)
                (unless first? (display "×"))
                (define factor (car factors))
                (define base (car factor))
                (define exponent (cadr factor))
                (cond
                  ((= exponent 1) (display base))
                  (else (printf "~s↑~s" base exponent)))
                (loop (cdr factors) #f))))
          (get-output-string str-port))))))
#;
(define (index->row/col index)
  (define-values (r c) (quotient/remainder index 9))
  (list (add1 r) (add1 c)))

(define board-ref
  (case-lambda
    ((row col) (vector-ref board (row/col->index row col)))
    ((index) (vector-ref board index))))

(define board-set!
  (case-lambda
    ((row col value) (vector-set! board (row/col->index row col) value))
    ((index value) (vector-set! board index value))))

(define (fill-board input)
  (unless (board? input) (raise-argument-error 'sudoku "board?" input))
  (set! board (make-vector N↑2)) ; Initially filled with zeros.
  (for ((index in-indices) (d (in-list input)))
    (when (digit? d) ; Leave empty fields zero.
      (define ns (vector-ref neighbours index))
      (for ((n (in-list ns)))
        (when (= d (board-ref n))
          (error 'sudoku "same neigbouring digit ~s at indices ~s and ~s" d n index)))
      (board-set! index d))))

(define (print-board (solution? #f))
  (when (and solution? (zero? nr-of-solutions)) (displayln "Solution(s)\n"))
  (for ((row in-digits))
    (for ((col in-digits))  (printf "~s " (convert-zero (board-ref row col))))
    (newline))
  (newline))

;;   (require fmt/fmt)
;;   (define board-fmt (fmt 'cur "USL2 3(_3 3(_3 3 (_ 3 3 D 2X)/)/)"))
;;   (define (print-board (solution? #f))
;;     (when (and solution? (zero? nr-of-solutions)) (displayln "Solution(s)\n"))
;;     (board-fmt board))

; Two fields are neighbours if in the same row, the same column or the same subboard. neighbours is
; a vector of N↑2 elements, element i being a list of the indices of all neighbours of field i.

(define neighbours
  (let ((neighbour-vector (build-vector N↑2 (λ (index) (mutable-seteq)))))
    (for* ((row in-digits) (col in-digits))
      (for ((r in-digits) #:unless (= r row))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r col)))
      (for ((c in-digits) #:unless (= c col))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index row c)))
      (let*
        ((r (add1 (* K (quotient (sub1 row) K))))
         (c (add1 (* K (quotient (sub1 col) K)))))
        (for* ((r (in-range r (+ r K))) (c (in-range c (+ c K))) #:unless (and (= r row) (= c col)))
          (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r c)))))
    ; Convert to immutable vector with lists for speeding up.
    (apply vector-immutable (map set->list (vector->list neighbour-vector)))))

(define (find-least-empty-field/digits empty-fields)
  (define free-digit-lists (find-free-digits empty-fields))
  (cond
    ((not free-digit-lists) (values #f #f)) 
    (else
      (find-least-empty-field/digits-help
        (car empty-fields)
        (car free-digit-lists)
        (cdr empty-fields)
        (cdr free-digit-lists)
        (length (car free-digit-lists))))))

(define (find-least-empty-field/digits-help f ds empty-fields free-digit-lists n)
  (cond
    ((null? empty-fields) (values f ds))
    (else
      (define new-f (car empty-fields))
      (define new-dgts (car free-digit-lists))
      (define new-n (length new-dgts))
      (cond
        ((= new-n 1) (values new-f new-dgts))
        ((< new-n n)
         (find-least-empty-field/digits-help
           new-f new-dgts (cdr empty-fields) (cdr free-digit-lists) new-n))
        (else
          (find-least-empty-field/digits-help
            f ds (cdr empty-fields) (cdr free-digit-lists) n))))))

(define (find-free-digits empty-fields)
  (let/cc exit
    (for/list ((field (in-list empty-fields)))
      (define free-digits
        (for/list
          ((d in-digits)
           #:when (for/and ((neighbour (in-list (vector-ref neighbours field))))
                    (not (= (board-ref neighbour) d))))
          d))
      (if (null? free-digits) (exit #f) free-digits))))

;=====================================================================================================
