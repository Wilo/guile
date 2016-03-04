(use-modules
 ;pattern matching
 (ice-9 match)
 ;list manipulations
 (srfi srfi-1))

; see gameoflife_test.scm for running
(define (game-of-life)
  ; initialisation
  (let ((grid #nil)
        (width #nil)
        (height #nil))
    ; private properties
    (define (getgrid) grid)
    (define (setgrid! new-grid)
      (match (storegrid new-grid)
             ((new-height new-width new-grid)
              (set! grid new-grid)
              (set! height new-height)
              (set! width new-width))))
    (define (step!)
      (set! grid (updategrid grid height width)))
    ; public interface
    (define (self message)
      (cond ((eqv? message 'getgrid) getgrid)
            ((eqv? message 'setgrid!) setgrid!) ;sets up a new grid
            ((eqv? message 'step!) step!) ;
            (else (error "Undefined message" message))))
    self))

;---- internal function implementations

; use arrays internally for lookup performance: https://www.gnu.org/software/guile/manual/html_node/Array-Procedures.html#Array-Procedures
(define (storegrid grid)
  (let ((height (length grid))
        (width (length (car grid)))
        (grid-as-array (list->array 2 grid)))
    (list height width grid-as-array)))

; rules here
(define (updategrid grid height width)
  (letrec ((newgrid (make-array 0 height width))
           (update-cell (lambda (old-cell i j)
                          (let ((num-neighbors (live-neighbors i j)))
                            (cond ((eq? old-cell 1) ;live cells survive with 2/3 live neighbours
                                   (cond ((or (= num-neighbors 2) (= num-neighbors 3)) 1)
                                         (else 0)))
                                  (else (cond ((= num-neighbors 3) 1) ; dead cells reanimate with 3 live neighbours
                                              (else 0)))))))
           (live-neighbors (lambda (i j)
                             (+ (neighbor-count (- i 1) (- j 1))
                                (neighbor-count (- i 1) j)
                                (neighbor-count (- i 1) (+ j 1))
                                (neighbor-count i (- j 1))
                                (neighbor-count i (+ j 1))
                                (neighbor-count (+ i 1) (- j 1))
                                (neighbor-count (+ i 1) j)
                                (neighbor-count (+ i 1) (+ j 1)))))
           (neighbor-count (lambda (i j)
                             (cond ((< i 0) 0)
                                   ((>= i height) 0)
                                   ((< j 0) 0)
                                   ((>= j width) 0)
                                   (else (array-ref grid i j))))))
    (list->array 2 (map (lambda (i)
                         (map (lambda (j)
                                (update-cell (array-ref grid i j) i j))
                              (iota width)))
                         (iota height)))))

;---- other functions

; pretty grid print
;[[int]] -> ()
(define* (pretty-print-grid grid #:optional (live #\o) (dead #\-))
  (let ((int->cell (lambda (int)
                     (cond ((= int 1) live)
                           (else dead)))))
    (for-each (lambda (row)
                (for-each (lambda (int)
                            (display (int->cell int)))
                          row)
                (display #\newline))
              grid)))


; filename -> [[int]]
; file should contain an rectangular array of characters. The 'alive' character representation can be optionally set (assumed to be 'o' (lowercase O))
(define* (grid-from-file infile #:optional (live #\o))
  (let* ((char->cell (lambda (char)
                       (cond ((eq? char live) 1)
                             (else 0))))
        (row->cells (lambda (row)
                      (map (lambda (char)
                             (char->cell char))
                           (string->list row)))))
    (filter (compose not null?)
            (map (lambda (row)
                   (row->cells row))
                 (string-split
                   (string-trim
                     (call-with-input-file infile read-string))
                   #\newline)))))

;---- OOP sugar functions
(define (send message object . args)
  (let ((method (object message)))
    (cond ((procedure? method) (apply method args))
          (else (error "Error in method lookup " method)))))

(define (new-instance class . parameters)
  (apply class parameters))

;---- Helper functions
(define (compose f g) (lambda (x) (f (g x))))
