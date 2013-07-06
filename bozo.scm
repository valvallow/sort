(use srfi-43)
(use srfi-27)

(define (bozo-sort ls)
  (random-source-randomize! default-random-source)
  (let ((vect (list->vector ls))
        (len (length ls)))
    (until (apply < (vector->list vect))
      (vector-swap! vect
                    (random-integer len)
                    (random-integer len)))
    (vector->list vect)))


(use gauche.sequence)

(dotimes (10)
  (let1 data (shuffle (iota 10))
    (time (bozo-sort data))
    (print)))

;(time (bozo-sort data))
; real  30.852
; user  30.740
; sys    0.000

;(time (bozo-sort data))
; real   0.862
; user   0.860
; sys    0.000

;(time (bozo-sort data))
; real   5.286
; user   5.280
; sys    0.000

;(time (bozo-sort data))
; real   0.598
; user   0.600
; sys    0.000

;(time (bozo-sort data))
; real   2.561
; user   2.560
; sys    0.000

;(time (bozo-sort data))
; real   4.554
; user   4.540
; sys    0.000

;(time (bozo-sort data))
; real   4.434
; user   4.420
; sys    0.000

;(time (bozo-sort data))
; real  12.168
; user  12.130
; sys    0.000

;(time (bozo-sort data))
; real  11.710
; user  11.670
; sys    0.000

;(time (bozo-sort data))
; real  20.846
; user  20.790
; sys    0.000

