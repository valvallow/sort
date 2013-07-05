(use srfi-1)

(define (quick-sort ls)
  (if (null? ls)
      '()
      (receive (ws ys)
          (partition (pa$ > (car ls))(cdr ls))
        (append (quick-sort ws)
                (list (car ls))
                (quick-sort ys)))))


(use gauche.sequence)
(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))

(test quick-sort 6)
; length = 100
;(time (sorter ls))
; real   0.001
; user   0.000
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.004
; user   0.000
; sys    0.010

; length = 10000
;(time (sorter ls))
; real   0.060
; user   0.060
; sys    0.000

; length = 100000
;(time (sorter ls))
; real   0.737
; user   0.730
; sys    0.000

; length = 1000000
;(time (sorter ls))
; real   9.191
; user   9.110
; sys    0.050

; length = 10000000
;(time (sorter ls))
; real 124.532
; user 123.700
; sys    0.450

