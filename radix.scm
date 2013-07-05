(define (radix-sort ls :optional (base 10))
  (define (digit-count num)
    (x->integer (ceiling (/ (log num)(log base)))))
  (define (digit-of index num)
    (modulo (quotient num (expt base index)) base))
  (define (list-set! ls n obj)
    (let/cc hop
      (let rec ((l ls)(n n))
        (if (zero? n)
            (begin (set! (car l) obj)
                   (hop ls))
            (rec (cdr l)(- n 1))))))
  (set! (setter list-ref) list-set!)
  (let1 count (digit-count (abs (apply max ls)))
    (let rec ((ls ls)(index 0))
      (if (< index count)
          (let1 buckets (make-list base '())
            (for-each
             (^e (let1 digit (digit-of index e)
                   (set! (list-ref buckets digit)
                         (cons e (list-ref buckets digit)))))
             ls)
            (rec (apply append (map reverse buckets))(+ index 1)))
          ls))))

(radix-sort (shuffle (iota 10)))

(use gauche.sequence)
(define (test sorter n)
  (for-each (^i (let1 ls (shuffle (iota (expt 10 i)))
                  (print "; length = " (expt 10 i))
                  (time (sorter ls))
                  (print)))
            (iota n 2)))


(test radix-sort 6)
; length = 100
;(time (sorter ls))
; real   0.000
; user   0.000
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.004
; user   0.000
; sys    0.000

; length = 10000
;(time (sorter ls))
; real   0.084
; user   0.080
; sys    0.000

; length = 100000
;(time (sorter ls))
; real   1.034
; user   1.030
; sys    0.010

; length = 1000000
;(time (sorter ls))
; real  13.469
; user  13.400
; sys    0.040

; length = 10000000
;(time (sorter ls))
; real 242.686
; user 241.360
; sys    0.610
