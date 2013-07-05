(define (bucket-sort ls)
  (let* ((min (apply min ls))
         (max (apply max ls))
         (buckets (make-vector (+ (- max min) 1) '())))
    (for-each (lambda (e)
                (let ((i (- e min)))
                  (vector-set! buckets i
                               (cons e (vector-ref buckets i)))))
                ls)
    (apply append (vector->list buckets))))


;;
;; test
;;

(use gauche.sequence)

(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))


(test bucket-sort 6)
; length = 100
;(time (sorter ls))
; real   0.000
; user   0.000
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.000
; user   0.000
; sys    0.000

; length = 10000
;(time (sorter ls))
; real   0.003
; user   0.010
; sys    0.000

; length = 100000
;(time (sorter ls))
; real   0.043
; user   0.040
; sys    0.000

; length = 1000000
;(time (sorter ls))
; real   1.032
; user   0.490
; sys    0.090

; length = 10000000
;(time (sorter ls))
; real   8.767
; user   8.490
; sys    0.240
