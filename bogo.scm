(use gauche.sequence)

(define (bogo-sort ls)
  (if (apply < ls)
      ls
      (bogo-sort (shuffle ls))))


(define data-5 (shuffle (iota 5)))
(define data-10 (shuffle (iota 10)))
(define data-15 (shuffle (iota 15)))

(time (bogo-sort data-5))
;(time (bogo-sort data-5))
; real   0.001
; user   0.000
; sys    0.000

(time (bogo-sort data-10))
;(time (bogo-sort data-10))
; real  12.053
; user  12.010
; sys    0.000


