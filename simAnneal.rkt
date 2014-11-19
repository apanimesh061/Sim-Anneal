#lang racket
(require racket/file)

;; reads the adjacency matrix from a DAT file
(define adj-mat 
  (map 
   (λ(l) 
     (map 
      (λ(i)
        (string->number i))
      (string-split l)))
   (file->lines "adjmat.dat")))

;; gives the final representation of the adjacency matrix
(define (mat-index l1 l2)
  (foldr
   append
   empty
   (map
    (λ(e1)
      (list 
       (map
        (λ(e2) 
          (list e1 e2))
        (build-list l1 identity))))
    (build-list l2 identity))))

(define currentOrder 
  (build-list (length adj-mat) identity))
(define shortestDistance 0.0)
(define adj-mat-index 
  (mat-index (length adj-mat) (length adj-mat)))
(define iteration -1)
(define temperature 10000.0)
(define coolingRate 0.9999)
(define absTemp 0.00001)

(define final-mat 
  (map 
   (λ(r1 r2) 
     (map 
      (λ(i j) 
        (list i j)) 
      r1 r2)) 
   adj-mat-index adj-mat))

(define (get-pairs lst)
  (map 
   (λ(i j) 
     (list i j)) 
   lst
   (append (rest lst) '(0))))

(define (get-total-cost seq)
  (foldr 
   + 
   0.0 
   (map 
    (λ(ind) 
      (list-ref (list-ref adj-mat (first ind)) (second ind)))
    (get-pairs seq))))

(define newOrder empty)
(define distance (get-total-cost currentOrder))
(define deltaDis (- (get-total-cost currentOrder) distance))

;; updating currentOrder and distance every iteration
(define (update-curr-dis)
  (set! currentOrder newOrder)
  (set! distance (+ deltaDis distance)))

(define (no-update-curr-dis)
  (set! currentOrder currentOrder)
  (set! distance distance))

(define (update-new-order)
  (set! newOrder (append '(0) (shuffle (rest currentOrder))))
  (set! deltaDis (- (get-total-cost newOrder) distance))
  ;; finding difference in energy
  (if (or (< deltaDis 0.0)
          (and (> distance 0.0)
               (> (exp (/ (* -1.0 deltaDis) temperature)) (random))))
      (update-curr-dis)
      (no-update-curr-dis))
  (set! temperature (* temperature coolingRate))
  (set! iteration (add1 iteration))
  (anneal))

(define (anneal)
  (cond
    [(> temperature absTemp) (update-new-order)]
    [else (set! shortestDistance distance)]))

;; start the annealing
(anneal)

;; result
(display (string-append "The shortest distance for TSP: "
                        (number->string shortestDistance)))
(display "\n")
(display (string-append "The shortest path for TSP: " 
                        (string-trim 
                         (apply 
                          string-append 
                          (map 
                           (λ(n) 
                             (string-append (number->string n) " ")) 
                           currentOrder)))))
(display "\n")
(display (string-append "Number of iterations: " (number->string iteration)))
