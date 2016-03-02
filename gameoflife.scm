(use-modules
 ;read-line
 (ice-9 rdelim)
 ;pattern matching
 (ice-9 match)
 ;list manipulations
 (srfi srfi-1))

; see gameoflife_test.scm for running
(define* (game-of-life #:optional (alive 'o) (dead '-))
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
    (define (step!) (set! grid (updategrid grid height width)))
    ; public interface
    (define (self message)
      (cond ((eqv? message 'getgrid) getgrid)
            ((eqv? message 'setgrid!) setgrid!) ;sets up a new grid
            ((eqv? message 'step!) step!) ;
            (else (error "Undefined message" message))))
    self))

;---- internal function implementations

; uses arrays internally for lookup performance: https://www.gnu.org/software/guile/manual/html_node/Array-Procedures.html#Array-Procedures
(define (storegrid grid)
  (let ((height (length grid))
        (width (length (car grid)))
        (grid-as-array (list->array 2 grid)))
    (list height width grid-as-array)))

(define (updategrid grid height width)
  (letrec ((newgrid (make-array 0 height width))
           (update-cell (lambda (old-cell i j)
                          (let ((num-neighbors (live-neighbors i j)))
                            (cond ((eq? old-cell 1)
                                   (cond ((or (= num-neighbors 2) (= num-neighbors 3)) 1)
                                         (else 0)))
                                  (else (cond ((= num-neighbors 3) 1)
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
    (for-each (lambda (i)
                (for-each (lambda (j)
                            (array-set! newgrid
                                        (update-cell 
                                          (array-ref grid i j)
                                          i j)
                                        i j))
                          (iota width)))
              (iota height))
    newgrid))

;---- Library/sugar functions
(define (send message object . args)
  (let ((method (object message)))
    (cond ((procedure? method) (apply method args))
          (else (error "Error in method lookup " method)))))

(define (new-instance class . parameters)
  (apply class parameters))
