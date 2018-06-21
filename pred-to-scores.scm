;;;; HELLO! I generate the measures in measures.txt.
;;;; BEFORE you load me, load lib, enc-testing, hmm, and pred.

; initialize counts
(define count-env (extend-top-level-environment user-initial-environment))
(vector-map
  (lambda (n)
    (eval (list 'define n 0) count-env))
  #(x0x0 x0x1 x0x2 x1x0 x1x1 x1x2 x2x0 x2x1 x2x2 x0 x1 x2))

; compare predicted state-sequences with actual state-sequences
; <compare!> takes two matrices and compares their respective entries. 
(define compare! (lambda (predicted actual)
  (if (zero? (vector-length predicted))
        'done
        (begin (vector-for-each
                 (lambda (pred act)
                   (if (= pred act)
                       (eval (list 'inc! (symbol 'x act)) count-env)
                       (eval (list 'inc! (symbol 'x act 'x pred)) count-env)))
                 (vector-head predicted) (vector-head actual))
                 (compare! (vector-behead predicted) (vector-behead actual))))))
(compare! prediction-matrix enc-state-matrix)

; define measures
(define tp (lambda (lit) (eval lit count-env)))
(define fp (lambda (lit)
  (eval (list '+ (symbol 'x0 lit) (symbol 'x1 lit) (symbol 'x2 lit)) count-env)))
(define fn (lambda (lit)
  (eval (list '+ (symbol lit 'x0) (symbol lit 'x1) (symbol lit 'x2)) count-env)))
(define precision (lambda (lit)
  (if (zero? (tp lit))
      1
      (exact->inexact (/ (tp lit) (+ (tp lit) (fp lit)))))))
(define recall (lambda (lit)
  (if (zero? (tp lit))
      1
      (exact->inexact (/ (tp lit) (+ (tp lit) (fn lit)))))))
(define f-measure (lambda (lit)
  (if (zero? (+ (precision lit) (recall lit)))
      1
      (/ (* 2 (precision lit) (recall lit)) 
         (+ (precision lit) (recall lit))))))
(define state-measure-list '(I B O Avg))
(define i (symbol 'x 2))
(define b (symbol 'x 0))
(define o (symbol 'x 1))
(define prec-i (precision i))
(define prec-b (precision b))
(define prec-o (precision o))
(define prec-avg (/ (+ prec-i prec-b prec-o) 3))
(define rec-i (recall i))
(define rec-b (recall b))
(define rec-o (recall o))
(define rec-avg (/ (+ rec-i rec-b rec-o) 3))
(define f-i (f-measure i))
(define f-b (f-measure b))
(define f-o (f-measure o))
(define f-avg (/ (+ f-i f-b f-o) 3))
(define percent (lambda (n) (* n 100.)))
(define prec-list (map percent (list prec-i prec-b prec-o prec-avg)))
(define rec-list (map percent (list rec-i rec-b rec-o rec-avg)))
(define f-list (map percent (list f-i f-b f-o f-avg)))
(define accuracy
  (percent (/ (eval '(+ x0 x1 x2) count-env)
              (eval '(+ x0x0 x0x1 x0x2 x1x0 x1x1 x1x2 x2x0 x2x1 x2x2 
                      x0 x1 x2)
                    count-env))))
(define error-list
  (map
    (lambda (lit)
      (eval lit count-env))
    '(x0x1 x0x2 x1x0 x1x2 x2x0 x2x1)))

; write out measures
(define scores (open-output-file "scores.txt" #t))
(set-current-output-port! scores)
(write-string 
  "\n================================================================================\nRESULTS FOR: Smoothing: ")
(write opt-smooth-type)
(write-string " at strength ")
(write opt-smooth-param)
(write-string ";\nLog-space: ")
(write opt-log)
(write-string "; Emissions: ")
(write opt-emit)
(write-string "; Double-count: ")
(write opt-double-count)
(write-string ".\n\nAccuracy: ")
(write accuracy)
(write-string "\nKey: ")
(write state-measure-list)
(write-string "\nPrecision: ")
(write prec-list)
(write-string "\nRecall: ")
(write rec-list)
(write-string "\nF-Measure: ")
(write f-list)
(write-string "\n(BpO BpI OpB OpI IpB IpO): ")
(write error-list)
(flush-output)
(close-all-open-files)

; THE END
