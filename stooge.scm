(use srfi-43)

(define (stooge-sort ls)
  (let1 vect (list->vector ls)
    (let rec ((head 0)(tail (- (vector-length vect) 1)))
      (when (< (vector-ref vect tail)
               (vector-ref vect head))
        (vector-swap! vect head tail))
      (when (<= 3 (+ (- tail head) 1))
        (let1 index-of-1/3 (quotient (+ (- tail head) 1) 3)
          (rec head (- tail index-of-1/3))
          (rec (+ head index-of-1/3) tail)
          (rec head (- tail index-of-1/3))))
      vect)))

(use gauche.sequence)
(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))

(test stooge-sort 2)

; length = 100
;(time (sorter ls))
; real   0.031
; user   0.030
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   7.330
; user   7.310
; sys    0.000
