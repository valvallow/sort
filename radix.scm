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


;; Gauche HEAD ver (remove list-set!)
(define (radix-sort ls :optional (base 10))
  (define (digit-count num)
    (x->integer (ceiling (/ (log num)(log base)))))
  (define (digit-of index num)
    (modulo (quotient num (expt base index)) base))
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
; real   0.001
; user   0.010
; sys    0.000

; length = 10000
;(time (sorter ls))
; real   0.018
; user   0.020
; sys    0.000

; length = 100000
;(time (sorter ls))
; real   0.235
; user   0.230
; sys    0.000

; length = 1000000
;(time (sorter ls))
; real   2.934
; user   2.880
; sys    0.040

; length = 10000000
;(time (sorter ls))
; real  36.419
; user  35.950
; sys    0.370



;; vector version
(use srfi-43)
(define (radix-sort ls :optional (base 10))
  (define (digit-count num)
    (x->integer (ceiling (/ (log num)(log base)))))
  (define (digit-of index num)
    (modulo (quotient num (expt base index)) base))
  (define (put-buckets! vect buckets index)
    (vector-for-each
     (^(_ e)
       (let1 digit (digit-of index e)
         (vector-set! buckets digit
                      (cons e (vector-ref buckets digit)))))
     vect))
  (let ((buckets (make-vector base '()))
        (v (list->vector ls)))
    (dotimes (index (digit-count (abs (apply max ls))))
      (put-buckets! v buckets index)
      (set! v (list->vector (apply append (map reverse (vector->list buckets)))))
      (set! buckets (make-vector base '())))
    (vector->list v)))

(test radix-sort 6)

; length = 100
;(time (sorter ls))
; real   0.000
; user   0.010
; sys    0.000

; length = 1000
;(time (sorter ls))
; real   0.001
; user   0.000
; sys    0.000

; length = 10000
;(time (sorter ls))
; real   0.015
; user   0.020
; sys    0.000

; length = 100000
;(time (sorter ls))
; real   0.203
; user   0.210
; sys    0.000

; length = 1000000
;(time (sorter ls))
; real   2.309
; user   2.300
; sys    0.000

; length = 10000000
GC Warning: Repeated allocation of very large block (appr. size 80003072):
	May lead to memory leak and poor performance.
GC Warning: Repeated allocation of very large block (appr. size 80003072):
	May lead to memory leak and poor performance.
;(time (sorter ls))
; real  65.467
; user  35.900
; sys    3.370

