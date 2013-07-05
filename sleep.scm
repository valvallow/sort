(use gauche.threads)

(define (sleep-sort ls)
  (let* ((result '())
         (threads (map (^n (thread-start!
                            (make-thread
                             (^ _ (thread-sleep! n)
                                (push! result n)))))
                       ls)))
    (for-each (pa$ thread-join!) threads)
    (reverse result)))


(define data (shuffle (iota 10)))
(time (sleep-sort data))
;(time (sleep-sort data))
; real   9.001
; user   0.010
; sys    0.000

