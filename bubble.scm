(define (bubble-sort ls)
  (define (pass1 ls)
    (let rec ((ls ls)(acc '()))
      (cond ((null? (cdr ls))(reverse (cons (car ls) acc)))
            ((< (cadr ls)(car ls))
             (rec (cons (car ls)(cddr ls))(cons (cadr ls) acc)))
            (else (rec (cdr ls)(cons (car ls) acc))))))
  (let rec ((ls ls)(i (- (length ls) 1)))
    (if (or (null? ls)(zero? i))
        ls
        (rec (pass1 ls) (- i 1)))))


;; vector, side effect
(define (bubble-sort! v)
  (let ((len (vector-length v)))
    (dotimes (_ len)
      (dotimes (i len)
        (when (< (+ i 1) len)
          (let ((cur (vector-ref v i))
                (next (vector-ref v (+ i 1))))
            (when (< next cur)
              (vector-set! v (+ i 1) cur)
              (vector-set! v i next)))))))
  v)

;;
;; sample
;;

(use gauche.sequence)

(define sample-100 (shuffle (iota 100)))
(define sample-1000 (shuffle (iota 1000)))
(define sample-10000 (shuffle (iota 10000)))

(time (print (bubble-sort sample-100)))
;(time (print (bubble-sort sample-100)))
; real   0.001
; user   0.000
; sys    0.000

(time (print (bubble-sort sample-1000)))
;(time (print (bubble-sort sample-1000)))
; real   0.128
; user   0.130
; sys    0.000

(time (print (bubble-sort sample-10000)))
;(time (print (bubble-sort sample-10000)))
; real  14.937
; user  12.080
; sys    0.000


(define sample-100v (list->vector sample-100))
(define sample-1000v (list->vector sample-1000))
(define sample-10000v (list->vector sample-10000))

(time (print (bubble-sort! sample-100v)))
;(time (print (bubble-sort! sample-100v)))
; real   0.001
; user   0.000
; sys    0.000

(time (print (bubble-sort! sample-1000v)))
;(time (print (bubble-sort! sample-1000v)))
; real   0.075
; user   0.080
; sys    0.000

(time (print (bubble-sort! sample-10000v)))
;(time (print (bubble-sort! sample-10000v)))
; real   7.479
; user   7.370
; sys    0.000



