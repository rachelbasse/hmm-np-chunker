;;;; HELLO! I generate the PD parameters to the HMM in hmm.scm.
;;;; BEFORE you load me, load lib and counts.

; <make-probs!> converts the raw counts to probabilities according to these 
; options:
; logopt =: 'yes | 'no ; use log-probabilities?
; emit =: 'state | 'transition ; use state or transition emission probabilities?
; double-count =: 'yes | 'no ; count occurrences of state B as also state I?
; smooth-type =: 'uniform-linear | 'uniform-square | 'uniform-cube | 'laplace 
;                | 'none ; type of smoothing
; smooth-param =: <real> ; strength of smoothing;
(define make-probs! (lambda (logopt emit double-count smooth-type . smooth-param)
  (define normalize (lambda (sum type)
    (lambda (count)
      (let* ((num-states (vector-length state-index))
             (num-obs-sym (vector-length obs-alph-index))
             (num-poss (cond ((equal? type 'init) num-states)
                             ((equal? type 'trans) num-states)
                             ((equal? type 'emit) num-obs-sym)
                             ((equal? type 'emit2) num-obs-sym)
                             ((equal? type 'emit2start) num-obs-sym)))
             (prob (if (zero? sum) 0 (/ count sum)))
             (smoothed
               (cond ((equal? smooth-type 'none) (+ prob (expt 10. -100)))
                     ((equal? smooth-type 'uniform-linear)
                         (- prob 
                            (* (if (null? smooth-param)
                                   .01
                                   (car smooth-param))
                               (- prob (/ num-poss)))))
                     ((equal? smooth-type 'uniform-square)
                         (- prob 
                            (* (if (null? smooth-param)
                                   .01
                                   (car smooth-param))
                               (- prob (/ num-poss))
                               (abs (- prob (/ num-poss))))))
                     ((equal? smooth-type 'uniform-cube)
                         (- prob 
                            (* (if (null? smooth-param)
                                   .01
                                   (car smooth-param))
                               (expt (- prob (/ num-poss)) 3))))
                     ((equal? smooth-type 'laplace)
                         (/ (+ count
                               (if (null? smooth-param)
                                   1
                                   (car smooth-param))) 
                            (+ sum
                               (if (null? smooth-param)
                                   num-poss
                                   (* (car smooth-param) num-poss)))))))
             (logged (if (equal? logopt 'yes) (log smoothed) smoothed)))
      logged))))
  ; make matrices for double-count option
  (define make-double-count (lambda (matrix)
    (vector
      (vector-map + (vector-ref matrix 0) (vector-ref matrix 0))
      (vector-ref matrix 1)
      (vector-map + (vector-ref matrix 0) (vector-ref matrix 2)))))
  (define emit-double-count-matrix (make-double-count emit-count-matrix))
  (define trans-double-count-matrix (make-double-count trans-count-matrix))
  (define emit2-double-count-matrix
    (vector
      (vector-map
        (lambda (row1 row2)
          (vector-map + row1 row2))
        (vector-ref emit2-count-matrix 0) (vector-ref emit2-count-matrix 0))
      (vector-ref emit2-count-matrix 1)
      (vector-map
        (lambda (row1 row2)
          (vector-map + row1 row2))
        (vector-ref emit2-count-matrix 0) (vector-ref emit2-count-matrix 2))))
  ; make probability-distribution matrices
  (define make-prob-1 (lambda (vect type)
    (let ((sum (eval (cons '+ (vector->list vect))
                         user-initial-environment)))
      (vector 
        (vector-map
          (normalize sum type)
          vect)))))
  (define make-prob-2 (lambda (matrix type)
    (vector-map
      (lambda (row)
        (let ((sum (eval (cons '+ (vector->list row))
                         user-initial-environment)))
          (vector-map
            (normalize sum type)
            row)))
      matrix)))
  (define make-prob-3 (lambda (3matrix type)
    (vector-map
      (lambda (row1)
        (vector-map
          (lambda (row2)
            (let ((sum (eval (cons '+ (vector->list row2))
                             user-initial-environment)))
              (vector-map
                (normalize sum type)
                row2)))
          row1))
      3matrix)))
  ;make probability procedures
  (make-matrix 'emit (make-prob-2 
                       (if (equal? double-count 'yes)
                           emit-double-count-matrix
                           emit-count-matrix)
                       'emit))
  (make-matrix 'emit2start (make-prob-2 emit2start-count-matrix 'emit2start))
  (make-matrix-3 'emit2 (make-prob-3
                          (if (equal? double-count 'yes)
                              emit2-double-count-matrix
                              emit2-count-matrix)
                          'emit2))
  (make-matrix 'init (make-prob-1 init-count-matrix 'init))
  (make-matrix 'trans (make-prob-2
                        (if (equal? double-count 'yes)
                            trans-double-count-matrix
                            trans-count-matrix)
                        'trans))
  ; write HMM params to file
  (define hmm (open-output-file "hmm.scm" #f))
  (set-current-output-port! hmm)
  (write-string "\n; counts->hmm.scm output\n(define opt-log '")
  (write logopt)
  (write-string ")\n(define opt-emit '")
  (write emit)
  (write-string ")\n(define opt-double-count '") 
  (write double-count)
  (write-string ")\n(define opt-smooth-type '")
  (write smooth-type)
  (write-string ")\n(define opt-smooth-param '")
  (write smooth-param)
  (write-string ")\n(make-matrix 'init ")
  (write (form-ref 'init))
  (write-string ")\n(make-matrix 'trans ")
  (write (form-ref 'trans))
  (write-string ")\n(make-matrix 'emit ")
  (write (form-ref 'emit))
  (write-string ")\n(make-matrix 'emit2start ")
  (write (form-ref 'emit2start))
  (write-string ")\n(make-matrix-3 'emit2 ")
  (write (form-ref 'emit2))
  (write-string ")\n")
  (flush-output)
  (close-all-open-files)))


;; THE END
