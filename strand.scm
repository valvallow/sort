(define (strand-sort ls)
  (define (merge ls1 ls2)
    (let rec ((ls1 ls1)(ls2 ls2)(acc '()))
      (cond ((null? ls1)
             (reverse (append (reverse ls2) acc)))
            ((null? ls2)
             (reverse (append (reverse ls1) acc)))
            ((< (car ls1)(car ls2))
             (rec (cdr ls1) ls2 (cons (car ls1) acc)))
            (else (rec ls1 (cdr ls2)(cons (car ls2) acc))))))
  (define (filter-sorted-elements ls)
    (if (null? ls)
        '()
        (let rec ((ls (cdr ls))(sorted (list (car ls)))(acc '()))
          (if (null? ls)
              (values (reverse sorted)
                      (reverse acc))
              (let1 sorted? (< (car sorted)(car ls))
                (rec (cdr ls)
                     (if sorted?
                         (cons (car ls)
                               sorted)
                         sorted)
                     (if sorted?
                         acc
                         (cons (car ls) acc))))))))
  ;; body
  (let rec ((ls ls)(acc '()))
    (if (null? ls)
        acc
        (receive (sorted rest)
            (filter-sorted-elements ls)
          (rec rest (merge sorted acc))))))


(use gauche.sequence)
(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))

(test strand-sort 5)
; length = 100
;(time (sorter ls))
; real   0.000
; user   0.000
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.005
; user   0.000
; sys    0.000

; length = 10000
;(time (sorter ls))
; real   0.215
; user   0.210
; sys    0.010

; length = 100000
;(time (sorter ls))
; real   6.727
; user   6.710
; sys    0.000

; length = 1000000
;(time (sorter ls))
; real 205.260
; user 204.550
; sys    0.100
