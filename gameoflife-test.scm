(use-modules
  (srfi srfi-64)
  ((game-of-life) #:renamer (symbol-prefix-proc 'gol:)))

(test-begin "game_logic")
(define gliders (gol:new-instance gol:game-of-life))
(gol:send 'setgrid! gliders (gol:grid-from-file "datasets/3x3.txt"))
(gol:send 'step! gliders)
(test-assert (equal? (gol:send 'getgrid gliders)
                     (list (list 0 0 1)
                           (list 1 1 0)
                           (list 0 1 0))))

(test-eqv (gol:send 'getgrid gliders)
          '('(0 0 1)
            '(1 1 0)
            '(0 1 0)))

(gol:send 'getgrid gliders)
