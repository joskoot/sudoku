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
including 9. There are 6670903752021072936960 complete boards. Theoretically they can be generated and
counted by calling procedure Sudoku with all fields empty, but in practice the large number makes this
impossible. For a feasible method to compute this number, see
https://people.math.sc.edu/girardi/Sudoku/enumerate.pdf.
See https://en.wikipedia.org/wiki/Sudoku and https://en.wikipedia.org/wiki/Mathematics_of_Sudoku too.

A Sudoku puzzle is well composed if it has one solution only and is minimally composed if it is well
composed and erasing one of the non empty fields yields a puzzle with more than one solution.
A minimally composed Sudoku puzzle has at least 17 non empty fields. Not all valid boards have a
solution. See file examples.rkt for a valid board with 5 non empty fields only but without solution. 

======================================================================================================

Procedure : (Sudoku ‹board›) --> natural?
‹board› : (and/c list? (λ (x) (= (length x) 81)))

A ‹board› is a list of 81 elements. Each subsequent contiguous sublist of 9 elements of ‹board› is
considered to be a row. Corresponding elements of rows form columns. Argument ‹board› is checked to
satisfy the restrictions. A field containing something else than an exact integer number n with 1≤n≤9,
is considered to be empty. The ‹board› does not necessarily represent a well composed puzzle with one
solution only. If parameter max-nr-of-solutions is #f, all solutions are computed. The solutions are
counted and if parameter print-solutions is #t they are printed too. Parameter max-nr-of-solutions
limits the number of solutions to be looked for. If parameter print-solutions is #f or the current
output port does not gather output in memory, the procedure runs in constant space. With parameter
print-solutions set to #t and the current output port connected to a file, memory consumption remains
bound, but the file may become very long or even exceed the space available in the output device.
With the parameter set to #t and output gathered in memory the number of solutions may be too large to
fit in memory. However, by definition a well composed puzzle has one solution only. Procedure Sudoku
returns the number of computed solutions.

======================================================================================================

Parameter : (print-solutions) --> boolean?
            (print-solutions ‹yes/no›) --> void?
‹yes/no› : any/c, initial value : #t

If this parameter is true, solutions are not printed.
Argument ‹yes/no› is coerced to a Boolean: (and ‹yes/no› #t).

======================================================================================================

Parameter : (max-nr-of-solutions) --> (or/c #f natural?)
            (max-nr-of-solutions ‹n›) --> void?
‹n› : (or/c #f natural?) initial value : #f

If this parameter is #f all solutions are looked for, else at most ‹n› solution are looked for.

======================================================================================================

The board is kept in a mutable vector. A row index r and column index c are converted to a vector
index as follows:

   index = 9(r‒1) + (c‒1)

Empty fields are replaced by 0 and printed as ‘•’. Variable neighbours contains a vector of 81 lists
of indices, the list at index i containing the indices of fields that must not contain the same digit
as field i. Each field has 20 neighbours, 8 in its row, 8 in its column and 4 additional ones in its
subboard. Procedure solver does a two level loop, the outer loop along empty fields and the inner
loop along all digits still allowed in the currently selected empty field. The outer loop selects a
field with the least number of digits still not in a neighbour. This speeds up by reducing the number
of cycles in the inner loop, although some time is lost because during every cycle of the outer loop
the field to be selected must be looked for. Much more time is saved than lost.

====================================================================================================|#

#lang racket/base

(require
  (only-in racket
    natural?
    seteq set->list mutable-seteq set-add!
    contract-out any/c and/c or/c -> case->
    sqr ~r)
  (only-in math/number-theory factorize))

(provide print-counter
  (contract-out
    (Sudoku (-> board? natural?))
    (print-solutions Bool-parameter)
    (max-nr-of-solutions Max-parameter)
    #;(board? (-> any/c boolean?))))

(define-syntax-rule (Parameter in out) (and/c parameter? (case-> (-> in void?) (-> out))))
(define Bool-parameter (Parameter any/c boolean?))
(define Max-parameter (Parameter (or/c #f natural?) (or/c #f natural?)))

; Dimensions for 9×9 board.
; K can be adapted for a board of N rows by N columns with N=K↑2, a total of N↑2 fields and subboards
; of N fields. Allowed are elements d, 1≤d≤N.

(define K 3)
(define N (sqr K))
(define N↑2 (sqr N))

; Top level variables:

(define solutions-counter 0)
(define nr-of-guesses 0)

; Helpers:

(define (coerce-to-boolean x) (and x #t))
(define print-solutions (make-parameter #t coerce-to-boolean 'parameter:print-solutions))
(define max-nr-of-solutions (make-parameter #f values 'parameter:max-nr-of-solutions))
(define print-counter (make-parameter #f values 'print-counter))
(define in-indices (in-range N↑2))
(define in-digits (in-range 1 (add1 N)))
(define in-rows in-digits)
(define in-cols in-digits)
(define (digit? d) (and (natural? d) (<= 1 d N)))
(define (row/col->index row col) (+ (* N (sub1 row)) (sub1 col)))
(define (convert-zero d) (if (digit? d) d '•))
(define board "To be initialized by procedure fill-board")
(define (board? obj) (and (list? obj) (= (length obj) N↑2)))
(define (~r3 x) (~r #:precision '(= 3) x))

; Main procedure:

(define (Sudoku board)
  (fill-board board)
  (displayln "\nStarting Sudoku")
  (unless (print-solutions) (displayln "Solutions not printed"))
  (define max (max-nr-of-solutions))
  (if max
    (printf
      "Looking for ~s solution(s) only (ignoring possible more solutions)~n"
      max)
    (displayln "Looking for all solutions"))
  (displayln "Initial board:\n")
  (print-board)
  (define-values (results cpu real gc) (time-apply solve '()))
  (displayln "Finishing Sudoku")
  (cond
    ((zero? solutions-counter)
     (displayln "No solution found")
     (printf "Nr of guesses ~s~n" nr-of-guesses))
    (else
      (printf "Nr of solutions: ~s ~a~n" solutions-counter (factors solutions-counter))
      (printf "Total nr of guesses: ~s ~a~n"nr-of-guesses (factors nr-of-guesses))
      (when (> solutions-counter 1)
        (printf "Mean nr of guesses per solution: ~a~n"
          (~r3 (/ nr-of-guesses solutions-counter))))))
  (printf "Times: cpu ~s ms, real ~s ms, gc ~s ms~n" cpu real gc)
  (when (>  solutions-counter 1)
    (printf "Mean cpu  time per solution: about ~a ms~n"
      (~r3 (/ cpu solutions-counter)))
    (printf "Mean real time per solution: about ~a ms~n"
      (~r3 (/ real solutions-counter))))
  (newline)
  solutions-counter)

; The solver:

(define (solve)
  (let/cc exit
    (set! solutions-counter 0)
    (set! nr-of-guesses 0)
    (define (solve empty-fields)
      (cond
        ((null? empty-fields)
         (when (print-solutions)
           (when (zero? solutions-counter) (displayln "Solution(s)\n"))
           (print-board))
         (set! solutions-counter (add1 solutions-counter))
         (when (and (print-counter) (zero? (modulo solutions-counter (print-counter))))
           (writeln solutions-counter))
         (define max (max-nr-of-solutions))
         (when (and max (>= solutions-counter max))
           (displayln "Possibly there are more solutions.")
           (exit)))
        (else
          (define-values (field digits) (find-empty-field/digits empty-fields))
          (when field
            (for ((d (in-list digits)))
              (set! nr-of-guesses (add1 nr-of-guesses))
              (board-set! field d)
              (solve (remove field empty-fields))
              (board-set! field 0))))))
    (solve (for/list ((index in-indices) #:unless (digit? (board-ref index))) index))))

; Factorize a number in a product of exponentiations of primes:

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

; Board procedures:

(define board-ref
  (case-lambda
    ((row col) (vector-ref board (row/col->index row col)))
    ((index) (vector-ref board index))))

(define board-set!
  (case-lambda
    ((row col value) (vector-set! board (row/col->index row col) value))
    ((index value) (vector-set! board index value))))

(define (fill-board input)
  (unless (board? input) (raise-argument-error 'Sudoku "board?" input))
  (set! board (make-vector N↑2)) ; Initially filled with zeros.
  (for ((index in-indices) (d (in-list input)))
    (when (digit? d) ; Leave empty fields zero.
      (define ns (vector-ref neighbours index))
      (for ((n (in-list ns)))
        (when (= d (board-ref n))
          (error 'Sudoku "invalid board: same neigbouring digit ~s at indices ~s and ~s" d n index)))
      (board-set! index d))))

(define (print-board)
  (for ((row in-rows))
    (for ((col in-cols))  (printf "~s " (convert-zero (board-ref row col))))
    (newline))
  (newline))

;;   (require fmt/fmt)
;;   (define board-fmt (fmt 'cur "USL2 3(_3 3(_3 3 (_ 3 3 D 2X)/)/)"))
;;   (define (print-board (solution? #f))
;;     (when (and solution? (zero? nr-of-solutions)) (displayln "Solution(s)\n"))
;;     (board-fmt board))

; Two fields are neighbours if in the same row, the same column or the same subboard. neighbours is
; a vector of N↑2 elements, element i being a list of the indices of all neighbours of element i.

(define neighbours
  (let ((neighbour-vector (build-vector N↑2 (λ (index) (mutable-seteq)))))
    (for* ((row in-rows) (col in-cols))
      (for ((r in-rows) #:unless (= r row))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r col)))
      (for ((c in-cols) #:unless (= c col))
        (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index row c)))
      (let*
        ((r (add1 (* K (quotient (sub1 row) K))))
         (c (add1 (* K (quotient (sub1 col) K)))))
        (for* ((r (in-range r (+ r K))) (c (in-range c (+ c K))) #:unless (and (= r row) (= c col)))
          (set-add! (vector-ref neighbour-vector (row/col->index row col)) (row/col->index r c)))))
    ; For speeding up convert to an immutable vector of lists.
    (apply vector-immutable (map set->list (vector->list neighbour-vector)))))

(define (find-empty-field/digits empty-fields)
  (define free-digits (find-free-digits empty-fields))
  (cond
    ((not free-digits) (values #f #f)) 
    (else
      (find-empty-field/digits-help
        (car empty-fields)
        (car free-digits)
        (cdr empty-fields)
        (cdr free-digits)
        (length (car free-digits))))))

(define (find-empty-field/digits-help f ds empty-fields free-digit-lists n)
  (cond
    ((null? empty-fields) (values f ds))
    (else
      (define new-f (car empty-fields))
      (define new-ds (car free-digit-lists))
      (define new-n (length new-ds))
      (cond
        ((= new-n 1) (values new-f new-ds))
        ((< new-n n)
         (find-empty-field/digits-help
           new-f new-ds (cdr empty-fields) (cdr free-digit-lists) new-n))
        (else
          (find-empty-field/digits-help
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
