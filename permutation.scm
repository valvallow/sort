(use util.combinations)

(define (permutation-sort ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (let rec ((candidates (permutations* ls)))
        (if (apply < (car candidates))
            (car candidates)
            (rec (cdr candidates))))))


(use gauche.sequence)
(dotimes (i 11)
  (let1 data (shuffle (iota i))
    (time (permutation-sort data))
    (print)))


;(time (permutation-sort data))
; real   0.000
; user   0.000
; sys    0.000

;(time (permutation-sort data))
; real   0.000
; user   0.000
; sys    0.000

;(time (permutation-sort data))
; real   0.000
; user   0.000
; sys    0.000

;(time (permutation-sort data))
; real   0.000
; user   0.000
; sys    0.000

;(time (permutation-sort data))
; real   0.000
; user   0.000
; sys    0.000

;(time (permutation-sort data))
; real   0.000
; user   0.000
; sys    0.000

;(time (permutation-sort data))
; real   0.002
; user   0.000
; sys    0.000

;(time (permutation-sort data))
; real   0.011
; user   0.010
; sys    0.000

;(time (permutation-sort data))
; real   0.084
; user   0.080
; sys    0.000

;(time (permutation-sort data))
; real   0.760
; user   0.750
; sys    0.010

;(time (permutation-sort data))
; real  10.210
; user  10.180
; sys    0.000
