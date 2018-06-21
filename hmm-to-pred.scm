;;; HELLO! I am the Viterbi algorithm. I generate a delta-matrix and best-path.
;;; BEFORE you load me, load lib, enc-testing, and hmm.

; <delta> takes a state and observation and returns the probability of
; the most-likely path ending in the state at the step given the observation.
(define delta (named-lambda (delta state obs)
  (vector-max-val
    (vector-map
      (lambda (prev-state) ((if (equal? opt-log 'yes) + *)  
                              (vector-ref prev-deltas prev-state)
                              (trans prev-state state)
                              (if (equal? opt-emit 'state)
                                  (emit state obs)
                                  (emit2 prev-state state obs))))
      state-index))))
(define delta-init (named-lambda (delta-init state obs)
  ((if (equal? opt-log 'yes) + *) 
     (init state)
     ((if (equal? opt-emit 'state) emit emit2start)
       state obs))))

; <psi> takes a current state and the deltas of the immediately previous states
; and returns the first previous state whose delta value maximizes the delta of
; the current state.
(define psi (named-lambda (psi state prev-deltas)
  (vector-head
    (vector-max-pos
      (vector-map
        (lambda (prev-state) ((if (equal? opt-log 'yes) + *) 
                                  (vector-ref prev-deltas prev-state)
                                  (trans prev-state state)))
        state-index)))))

; <make-delta-matrix!> takes an observation sequence and returns a
; matrix of deltas, where rows represent steps and columns represent states.
(define delta-matrix #())
(define prev-deltas #())
(define make-delta-matrix! (lambda (obs-seq)
  (define iter (lambda (obs-seq)
    (if (zero? (vector-length obs-seq))
        delta-matrix
        (begin
          (set! prev-deltas 
            (vector-map
              (lambda (state)
                (delta state (vector-head obs-seq)))
              state-index))
          (prefix! prev-deltas delta-matrix)
          (iter (vector-behead obs-seq))))))
  (gc-flip)
  ; calculate, set!, and save initial deltas
  (set! prev-deltas 
    (vector-map
      (lambda (state)
        (delta-init state (vector-head obs-seq)))
      state-index))
  (prefix! prev-deltas delta-matrix)
  ; calculate, set!, and save remaining deltas
  (iter (vector-behead obs-seq))))

; <make-best-path!> takes a delta matrix and returns the best path through it.
(define best-path #())
(define make-best-path! (lambda (delta-matrix)
  (define iter (lambda (delta-matrix best-state)
    (if (zero? (vector-length delta-matrix))
        best-path
        (begin
          (let* ((deltas (vector-head delta-matrix))
                (new-best-state (psi best-state deltas)))
            (prefix! new-best-state best-path)
            (iter (vector-behead delta-matrix) new-best-state))))))
  (let ((best-state (vector-head (vector-max-pos (vector-head delta-matrix)))))
    (prefix! best-state best-path)
    (iter (vector-behead delta-matrix) best-state))))

; generate predictions.
(define prediction-matrix
  (vector-map
    (lambda (obs-seq)
      (set! delta-matrix #())
      (set! best-path #())
      (make-delta-matrix! obs-seq)
      (make-best-path! delta-matrix))
    enc-obs-matrix))

; write predictions to file pred.scm
(define preds (open-output-file "pred.scm" #f))
(set-current-output-port! preds)
(write-string "\n(define prediction-matrix ")
(write prediction-matrix)
(write-string ")\n")
(flush-output)
(close-all-open-files)

;; THE END
