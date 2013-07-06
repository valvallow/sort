(define (bead-sort ls)
  (define (bead-down ls)
    (let rec ((ls (remove null? ls))(acc '()))
      (if (null? ls)
          (reverse acc)
          (rec (remove null? (map cdr ls))
               (cons (map car ls) acc)))))
  ;; body
  (map length (bead-down (map (cut make-list <> 1) ls))))



(use gauche.sequence)
(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))

(test bead-sort 3)

; length = 100
;(time (sorter ls))
; real   0.003
; user   0.000
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.242
; user   0.230
; sys    0.010

; length = 10000
;(time (sorter ls))
; real  28.459
; user  27.590
; sys    0.790
