(use srfi-43)
(define (selection-sort ls)
  (define (vector-min-index vect start-index)
    (do ((i start-index (+ i 1))
         (min-value +inf.0)
         (min-index 0))
        ((= i (vector-length vect))(values min-index min-value))
      (when (< (vector-ref vect i) min-value)
        (set! min-value (vector-ref vect i))
        (set! min-index i))))
  ;; body
  (do ((i 0 (+ i 1))
       (vect (list->vector ls)))
      ((= i (length ls))(vector->list vect))
    (vector-swap! vect i (vector-min-index vect i))))


(use gauche.sequence)
(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))

(test selection-sort 4)
; length = 100
;(time (sorter ls))
; real   0.001
; user   0.000
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.030
; user   0.030
; sys    0.000

; length = 10000
;(time (sorter ls))
; real   2.614
; user   2.610
; sys    0.000

; length = 100000
;(time (sorter ls))
; real 255.403
; user 254.710
; sys    0.020

