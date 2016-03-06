(use-modules
  (srfi srfi-64)
  ((game-of-life) #:renamer (symbol-prefix-proc 'gol:)))

(test-begin "game_logic")

;still life:
(define loaf (gol:new-instance gol:game-of-life))
(gol:send 'setgrid! loaf (list (list 0 1 1 0)
                               (list 1 0 0 1)
                               (list 0 1 0 1)
                               (list 0 0 1 0)))
(gol:send 'step! loaf)
(test-equal "loaf at t = 1"
            (gol:send 'getgrid loaf)
            (list (list 0 1 1 0)
                  (list 1 0 0 1)
                  (list 0 1 0 1)
                  (list 0 0 1 0)))

;oscillator:
(define toad (gol:new-instance gol:game-of-life))
(gol:send 'setgrid! toad (list (list 0 0 1 0)
                               (list 1 0 0 1)
                               (list 1 0 0 1)
                               (list 0 1 0 0)))
(gol:send 'step! toad)
(test-equal "toad at t = 1"
             (gol:send 'getgrid toad)
             (list (list 0 0 0 0)
                   (list 0 1 1 1)
                   (list 1 1 1 0)
                   (list 0 0 0 0)))
(gol:send 'step! toad)
(test-equal "toad at t = 2"
            (gol:send 'getgrid toad)
            (list (list 0 0 1 0)
                  (list 1 0 0 1)
                  (list 1 0 0 1)
                  (list 0 1 0 0)))

(test-end "game_logic")

(test-begin "utility_functions")

;pretty printing
(define loaf-grid (list (list 0 1 1 0)
                        (list 1 0 0 1)
                        (list 0 1 0 1)
                        (list 0 0 1 0)))
(test-equal "pretty print works with custom dead/alive chars"
            (gol:pretty-print-grid loaf-grid #\x #\o)
            "oxxo\nxoox\noxox\nooxo")

;loading from file
(define basic-grid (gol:grid-from-file "datasets/3x3.txt"))
(test-equal "loading a grid from file works"
            basic-grid
            (list (list 0 0 1)
                  (list 1 1 0)
                  (list 0 1 0)))


(test-end "utility_functions")
