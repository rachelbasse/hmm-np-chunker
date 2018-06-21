;;;; HELLO! I generate the counts in counts.scm.
;;;; BEFORE you load me, load lib, training, and hmm.

; make the emit-count, emit2-count, emit2start-count, init-count, and 
; trans-count matrices.
(define count-matrices ((lambda (state-matrix obs-matrix)
  (let ((states (form-ref 'state)) (alph (form-ref 'obs-alph)))
    (define iter (lambda (state-seq obs-seq prev-state)
      (if (> (vector-length state-seq) 0)
        (let ((state (vector-head state-seq))
               (obs (vector-head obs-seq)))
          (evinc! (symbol prev-state state) count-env)
          (evinc! (symbol state '> obs) count-env)
          (evinc! (symbol prev-state state '> obs) count-env)
          (iter (vector-behead state-seq) (vector-behead obs-seq) state)))))
    (define count-env (extend-top-level-environment user-initial-environment))

    ; initialize counter variables
    (vector-for-each
      (lambda (state1)
        (vector-for-each
          (lambda (state2)
            (vector-for-each
              (lambda (obs)
                (eval (list 'define (symbol state1 '> obs) 0) count-env)
                (eval (list 'define (symbol '<start> state1 '> obs) 0) count-env)
                (eval (list 'define (symbol state1 state2 '> obs) 0) count-env)
                (eval (list 'define (symbol '<start> state1) 0) count-env)
                (eval (list 'define (symbol state1 state2) 0) count-env))
              alph))
          states))
      states)

    ; count everything
    (vector-for-each
      (lambda (state-seq obs-seq)
        (iter state-seq obs-seq '<start>))
      state-matrix obs-matrix)

    ; make raw-count matrices
    (define emit-count-matrix
      (vector-map
        (lambda (state) 
          (vector-map
            (lambda (obs)
              (eval (symbol state '> obs) count-env))
            alph))
        states))
    (define emit2start-count-matrix 
      (vector-map
        (lambda (state)
          (vector-map
            (lambda (obs)
              (eval (symbol '<start> state '> obs) count-env))
            alph))
        states))
    (define emit2-count-matrix
      (vector-map
        (lambda (state1)
          (vector-map
            (lambda (state2)
              (vector-map
                (lambda (obs)
                  (eval (symbol state1 state2 '> obs)
                        count-env))
                alph))
            states))
        states))
    (define init-count-matrix
      (vector-map
        (lambda (state)
          (eval (symbol '<start> state) count-env))
        states))
    (define trans-count-matrix
      (vector-map
        (lambda (state1)
          (vector-map
            (lambda (state2)
              (eval (symbol state1 state2) count-env))
            states))
        states))
    (vector emit-count-matrix emit2start-count-matrix emit2-count-matrix 
      init-count-matrix trans-count-matrix))) state-matrix obs-matrix))

; write counts to file counts.scm
(define counts (open-output-file "counts.scm" #t))
(set-current-output-port! counts)
(write-string "(define emit-count-matrix ")
(write (vector-ref count-matrices 0))
(write-string ")\n(define emit2start-count-matrix ")
(write (vector-ref count-matrices 1))
(write-string ")\n(define emit2-count-matrix ")
(write (vector-ref count-matrices 2))
(write-string ")\n(define init-count-matrix ")
(write (vector-ref count-matrices 3))
(write-string ")\n(define trans-count-matrix ")
(write (vector-ref count-matrices 4))
(write-string ")\n")
(flush-output)
(close-all-open-files)
(define count-matrices)

;; THE END
