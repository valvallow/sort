(define (radix-sort ls :optional (base 10))
  (define (flatten tree)
    (fold-right (lambda (e acc)
                  (if (pair?  e)
                      (append (flatten e) acc)
                      (cons e acc)))
                '() tree))
  (define (list-set! ls n obj)
    (let/cc hop
      (let rec ((l ls)(n n))
        (if (zero? n)
            (begin (set! (car l) obj)
                   (hop ls))
            (rec (cdr l)(- n 1))))))
  (set! (setter list-ref) list-set!)

  (let* ((max (apply max ls))
         (count (x->integer (ceiling (/ (log (abs max))(log base))))))
    (let rec ((ls ls)(i 0))
      (if (< i count)
          (let ((buckets (make-list base '())))
            (for-each
             (lambda (e)
               (let ((digit (modulo (quotient e (expt base i)) base)))
                 (set! (list-ref buckets digit)(cons e (list-ref buckets digit)))))
             ls)
            (rec (flatten (map reverse buckets))(+ i 1)))
          ls))
    ))



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
