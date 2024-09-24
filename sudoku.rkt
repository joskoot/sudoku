#lang racket

#|
(procedure : (sudoku ‹str›)
‹str› : string as described below

The string must contain 81 elements separated by white space.
An element must not contain white space.
A digit is a natural number n (<= 1 n 9).
Each element other than digit represents the absence of a digit.
An absent or non-digit is printed as `•´.
All solutions are computed and printed.

A completed sudoku board has 81 fields in 9 rows and 9 colums,
each field containing a digit n (<= 1 n 9), however with the following restrictions:
  A column or row must not contain a duplicate digit.
  There are 9 disjunct subboards of 3 by 3 adjacent rows/columns and
  no duplicate digit is allowed in a subboard.

Argument ‹str› is checked to satisfy the restrictions.
|#

(require fmt/fmt) ; Install from https://github.com/joskoot/fmt

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

(define (index->row/col index)
  (let-values (((r c) (quotient/remainder index 9)))
    (values (add1 r) (add1 c))))

(define (index->row+col index)
  (let-values (((r c) (quotient/remainder index 9)))
    (list (add1 r) (add1 c))))

(define board-ref
  (case-lambda
    ((board row col) (vector-ref board (row/col->index row col)))
    ((board index) (vector-ref board index))))

(define board-set!
  (case-lambda
    ((board row col value) (vector-set! board (row/col->index row col) value))
    ((board index value) (vector-set! board index value))))

(define (list->board lst)
  (define board (make-board))
  (for ((index in-index) (elem (in-list lst))) (board-set! board index elem))
  board)

(define neighbours
  (let ((board (make-board (λ (index) (mutable-seteqv)))))
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

(define (board-string? obj)
  (and (string? obj)
    (= (count (compose not char-whitespace?) (string->list obj)) 81)))

(define (read-board str)
  (unless (board-string? str) (raise-argument-error 'read-board "board-string?" str))
  (define board (make-board))
  (define input (open-input-string str))
  (for ((index in-index))
    (define d (read input))
    (when (digit? d)
      (define ns (board-ref neighbours index))
      (for ((n (in-set ns)))
        (when (eqv? d (board-ref board n))
          (error 'read-board "neigbouring digit: ~s ~s ~s" d index n)))
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
  (define (solve open)
    (let loop ((open open))
      (cond
        ((null? open) (print-board board) (set! nr-of-solutions (add1 nr-of-solutions)))
        (else
          (define index (car open))
          (unless (digit? (board-ref board index))
            (for ((d in-digit))
              (unless (yet-in-neighbour? d index board)
                (board-set! board index d)
                (loop  (cdr open))
                (board-set! board index 0))))))))
  (solve (for/list ((index in-index) #:unless (digit? (board-ref board index))) index))
  nr-of-solutions)

(define (show x) (println x) x)

(define (yet-in-neighbour? d index board)
  (for/or ((i (in-set (board-ref neighbours index))))
    (= (board-ref board i) d)))

(define (sudoku str)
  (printf "Staring sudoku: inital board:~n~n")
  (define board (read-board str))
  (print-board board)
  (printf "Solutions:~n~n")
  (define nr-of-solutions (solve board))
  (printf "Finishing sudoku: nr of solutions: ~s~n~n" nr-of-solutions))


  



(sudoku
  "1 2 3   4 5 6   7 8 9
   4 5 6   7 8 9   1 2 3
   7 8 9   1 2 3   4 5 6

   2 3 1   5 6 4   8 9 7
   5 6 4   8 9 7   2 3 1
   8 9 7   2 3 1   5 6 4

   3 1 2   6 4 5   9 7 8
   6 4 5   9 7 8   3 1 2
   9 7 8   3 1 2   6 4 5
")


(sudoku
  "1 2 3   4 5 6   7 8 9
   4 5 6   7 8 9   1 2 3
   7 8 9   1 2 3   4 5 6

   2 3 1   5 6 4   8 9 7
   5 6 4   8 9 7   2 3 1
   8 9 7   2 3 1   5 6 4

   3 1 2   6 4 5   9 7 8
   • • •   • • •   • • •
   • • •   • • •   • • •
")
     
(sudoku
  "1 2 3   4 5 6   7 8 9
   4 5 6   7 8 9   1 2 3
   7 8 9   1 2 3   4 5 6

   2 3 1   5 6 4   8 9 7
   5 6 4   8 9 7   2 3 1
   8 9 7   2 3 1   5 6 4

   3 1 2   6 4 5   9 7 8
   • • •   • • •   3 1 2
   • • •   • • •   • • •
")
     
(sudoku
  "1 2 3   4 5 6   7 8 9
   4 5 6   7 8 9   1 2 3
   7 8 9   1 2 3   4 5 6

   2 3 1   5 6 4   8 9 7
   5 6 4   8 9 7   2 3 1
   8 9 7   2 3 1   5 6 4

   3 1 2   6 4 5   9 7 8
   • • •   • • •   3 • •
   • • •   • • •   • • •
")

(sudoku
  "• • •   5 2 4   • • 6
   9 3 •   • • •   • 7 •
   • • •   • • •   • • •
   • • •   • • 3   • • •
   2 8 •   • • •   • • •
   • • 1   7 • •   4 • •
   • • •   6 • •   1 • 7
   • 9 •   • • •   • 2 •
   • • 4   • 8 •   • 3 •")
     
     

