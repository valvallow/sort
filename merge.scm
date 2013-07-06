(use srfi-1)
(define (merge-sort ls)
  (define (merge ls1 ls2)
    (let rec ((ls1 ls1)(ls2 ls2)(acc '()))
      (cond ((null? ls1)
             (reverse (append (reverse ls2) acc)))
            ((null? ls2)
             (reverse (append (reverse ls1) acc)))
            ((< (car ls1)(car ls2))
             (rec (cdr ls1) ls2 (cons (car ls1) acc)))
            (else (rec ls1 (cdr ls2)(cons (car ls2) acc))))))
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (let1 split-index (quotient (length ls) 2)
        (receive (head tail)
            (split-at ls split-index)
          (merge (merge-sort head)
                 (merge-sort tail))))))



(use gauche.sequence)
(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))

(test merge-sort 6)


; length = 100
;(time (sorter ls))
; real   0.000
; user   0.000
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.002
; user   0.010
; sys    0.000

; length = 10000
;(time (sorter ls))
; real   0.025
; user   0.030
; sys    0.000

; length = 100000
;(time (sorter ls))
; real   0.288
; user   0.280
; sys    0.000

; length = 1000000
;(time (sorter ls))
; real   3.422
; user   3.370
; sys    0.030

; length = 10000000
;(time (sorter ls))
; real  40.993
; user  40.620
; sys    0.240
