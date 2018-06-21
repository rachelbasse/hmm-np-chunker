;;;; HELLO! I am the library of generic procedures.

;; Counters
; <make-counts> takes a vector and returns an association vector of pairs
; (type token-count) for the vector's components.
(define make-counts (lambda (vect)
  (define count-env (extend-top-level-environment user-initial-environment))
  (vector-map
    (lambda (entry)
      (eval (list 'define entry 0) count-env))
    vect)
  (vector-map
    (lambda (entry)
      (eval (list 'inc! entry) count-env))
    vect)
  (list->vector
    (map
      (lambda (mylist) (list->vector mylist))
      (environment-bindings count-env)))))

;; Encodings
; <encode> takes a message and a code and returns the message transformed
; by replacing each letter of the message by the position of that letter in
; the code.
(define encode (lambda (message code)
  (define encode-env (extend-top-level-environment user-initial-environment))
    (vector-map
      (lambda (pos)
        (eval (list 'define (vector-ref code pos) pos) encode-env))
      (make-index-vect (vector-length code)))
    (eval (cons 'vector (vector->list message)) encode-env)))

;; Forms
(define form-assoc-list '())
(define make-form (lambda (literal form)
  (set! form-assoc-list (cons (list literal form) form-assoc-list))))
(define form-ref (lambda (literal)
  (cadr (assoc literal form-assoc-list))))

;; Indexed Sets
(define make-indexed-set (lambda (literal vect)
  (eval (list 'define
              (string->symbol (string literal '-index))
              (make-index-vect (vector-length vect)))
        user-initial-environment)
  (make-name literal vect)
  (make-form literal vect)))

;; Indexes
(define make-index-vect (lambda (length)
  (define iter (lambda (vect i)
    (cond ((= i length) vect)
          (else (iter (vector-append vect (vector i)) (1+ i))))))
  (iter #() 0)))

;; Lists
(define max-cdrs (lambda (ass-list)
  (let ((build (list (list 'start -1))))
    (map
      (lambda (pair)
        (let ((val (cadr pair))
              (max-cur (cadar build)))
          (cond ((> val max-cur) 
                   (begin 
                     (set! build (list pair))
                     (set! max-cur val)))
                ((= val max-cur) (set! build 
                                       (append (list pair) build))))))
      ass-list)
    build)))

;; Matrices
(define make-matrix (lambda (literal matrix)
  (define matrix-proc
    (if (= (vector-length matrix) 1)
        (lambda (i) (vector-ref (vector-ref matrix 0) i))
        (lambda (i j) (vector-ref (vector-ref matrix i) j))))
  (eval (list 'define literal matrix-proc) user-initial-environment)
  (make-form literal matrix)))
(define make-matrix-3 (lambda (literal matrix)
  (define matrix-proc
    (lambda (i j k) (vector-ref (vector-ref (vector-ref matrix i) j) k)))
  (eval (list 'define literal matrix-proc) user-initial-environment)
  (make-form literal matrix)))

(define matrix-map (lambda (procedure matrix)
  (vector-map 
    (lambda (row)
      (vector-map 
        (lambda (entry)
          (procedure entry))
        row))
    matrix)))

(define matrix->vector (lambda (matrix)
  (define iter (lambda (matrix vect)
    (if (> (vector-length matrix) 0)
        (iter (vector-behead matrix) 
              (vector-append vect (vector-head matrix)))
        vect)))
  (iter matrix #())))

;; Names
; <make-name> takes a key and a vector and adds name entries.
(define name-keys #())
(define name-values #())
(define make-name (lambda (literal vect)
  (set! name-keys (vector-append name-keys (vector literal)))
  (set! name-values (vector-append name-values (vector vect)))))
(define name-ref (lambda (literal number)
  (vector-ref (vector-ref name-values (vector-ref-inv name-keys literal)) 
              number)))

;; Other
; identity
(define id (lambda (x) x))
; <inc!> takes a variable and destructively increments its value by 1.
(define-syntax inc!
  (syntax-rules ()
    ((inc! x)
      (set! x (1+ x)))))
(define-syntax evinc!
  (syntax-rules ()
    ((evinc! symbol env)
      (eval (list 'inc! symbol) env))))
(define scale (lambda (scalee scale-to)
  (/ scale-to scalee)))

;; Vectors
; <vector-assoc> takes an association vector and a value and returns the second
; component of the first vector in the association vector whose first component
; is the value or #f if unfound.
(define vector-assoc (lambda (vect key)
  (let* ((test (vector-head vect))
         (test-key (vector-head test))
         (value (vector-ref test 1)))
    (cond ((zero? (vector-length vect)) #f)
          ((equal? key test-key) value)
          (else (vector-assoc (vector-behead vect) key))))))

; <vector-no-repeats> takes a vector and removes the repeated components.
(define vector-no-repeats (lambda (vect)
  (define repeat-env (extend-top-level-environment user-initial-environment))
    (vector-map
      (lambda (entry)
        (eval (list 'define entry 0) repeat-env))
      vect)
    (list->vector
      (map
        (lambda (pair) (car pair))
        (environment-bindings repeat-env)))))

; <vector-repulse> takes two vectors of numbers and pushes their values apart
; by some fraction of the original values.
(define vector-repulse (lambda (vect1 vect2 strength)
  (define iter (lambda (vect1 vect2 vect1s vect2s vect1new vect2new strength)
    (if (zero? (vector-length vect1))
        (cons vect1new vect2new)
        (let ((s1 (vector-head vect1s))
              (s2 (vector-head vect2s))
              (o1 (vector-head vect1))
              (o2 (vector-head vect2)))
          (iter (vector-behead vect1)
                (vector-behead vect2)
                (vector-behead vect1s)
                (vector-behead vect2s)
                (vector-suffix vect1new 
                               ((if (>= s1 s2) + -) o1 (round->exact (* o1 strength))))
                (vector-suffix vect2new 
                               ((if (>= s2 s1) + -) o2 (round->exact (* o2 strength))))
                strength)))))
  (let* ((scaled (vector-scale vect1 vect2))
        (vect1s (car scaled))
        (vect2s (cdr scaled)))
    (iter vect1 vect2 vect1s vect2s #() #() strength))))


; <vector-scale> takes two vectors of numbers and returns them with the smaller scaled
; up to the size of the larger one. It returns the vectors in order in a cons cell.
(define vector-scale (lambda (vect1 vect2)
  (let* ((sum1 (eval (vector->list (vector-prefix '+ vect1)) user-initial-environment))
         (sum2 (eval (vector->list (vector-prefix '+ vect2)) user-initial-environment))
         (k (if (>= sum1 sum2)
                (floor (/ sum1 sum2))
                (floor (/ sum2 sum1))))
         (scaled (lambda (vect)
                   (vector-map (lambda (n) (* n k)) vect))))
    (if (>= sum1 sum2)
        (cons vect1 (scaled vect2))
        (cons (scaled vect1) vect2)))))

; <vector-suffix> takes two vectors and returns a vector whose last component
; is the second vector and whose other components are the unchanged components
; of the first vector.
(define vector-suffix (lambda (vect1 vect2)
  (vector-append vect1 (vector vect2))))
(define vector-prefix (lambda (vect1 vect2)
  (vector-append (vector vect1) vect2)))
(define-syntax suffix!
  (syntax-rules ()
    ((suffix! vect1 vect2)
      (set! vect1 (vector-suffix vect1 vect2)))))
(define-syntax prefix!
  (syntax-rules ()
    ((prefix! vect1 vect2)
      (set! vect2 (vector-prefix vect1 vect2)))))

(define vector-head (lambda (vect)
  (vector-ref vect 0)))
(define vector-behead (lambda (vect)
  (let ((len (vector-length vect)))
    (if (= len 0)
        #()
        (do ((i 1 (1+ i)) (copy #()))
          ((= i len) copy)
          (suffix! copy (vector-ref vect i)))))))

; <vector-max> takes a vector of numbers and returns a vector whose first 
; component is the maximum value in the vector and whose second component is
; a vector of the positions at which the maximum value occurs.
(define vector-max-val (lambda (vect)
  (vector-ref (vector-max vect) 0)))
(define vector-max-pos (lambda (vect)
  (vector-ref (vector-max vect) 1)))
(define vector-max (lambda (vect)
  (do ((i 0 (1+ i)) (cur-max (vector-head vect)) (max-pos #()))
    ((= i (vector-length vect)) (vector cur-max max-pos))
    (let ((cur-val (vector-ref vect i)))
      (cond ((> cur-val cur-max)
              (begin (set! cur-max cur-val)
                     (set! max-pos (vector i))))
            ((= cur-val cur-max)
              (set! max-pos (vector-suffix max-pos i))))))))

; <vector-ref-inv> takes a vector and value and returns the first position of
; the vector that has the value or #f if unfound.
(define vector-ref-inv (lambda (vect val)
  (define iter (lambda (vect i)
       (cond ((= i (vector-length vect)) #f)
             ((equal? val (vector-ref vect i)) i)
             (else (iter vect (1+ i))))))
  (iter vect 0)))

; <vector-union> takes two vectors and returns the first vector with the second
; suffixed to it minus any components occurring in the first.
(define vector-union (lambda (vect1 vect2)
  (define iter (lambda (vect1 vect2 i)
    (cond ((zero? (vector-length vect2)) vect1)
          ((equal? i (vector-length vect1))
            (iter (vector-suffix vect1 (vector-head vect2))
                  (vector-behead vect2)
                  0))
          ((equal? (vector-head vect2) (vector-ref vect1 i))
             (iter vect1 (vector-behead vect2) 0))
          (else (iter vect1 vect2 (1+ i))))))
  (iter vect1 vect2 0)))

;;; HMM Params:
; state = vector of states.
; obs-alph = vector of observation symbols.
; init = matrix of initial probabilities.
; (init i) is the probability of starting in state i.
; emit = matrix of state emission probabilities.
; (emit i j) is the probability of state i emitting observation j.
; emit2 = matrix of transition emission probabilities.
; (emit2 i j k) is the probability of state j emitting observation k after
; being in state i.
; emit2start = matrix of initial transition emission probabilities.
; (emit2start i j) is the probability of state i emitting observation j at
; the beginning of the sequence. 
; trans = matrix of first-order transition probabilities.
; (trans i j) is the probability of transitioning from state i to state j.

(make-indexed-set 'state #(b o i))

; Use this to recreate if alphabet changes.
; (make-indexed-set 'obs-alph (vector-no-repeats (matrix->vector obs-matrix)))
(make-indexed-set 'obs-alph #(uh sym wp<dollar> fw <colon> rp pdt rbs wrb wdt
jjr rbr wp jjs prp nnps <rparen> <dollar> <lparen> <hash> md ex vbd <apost>
<quote> cd prp<dollar> vbg vbp <period> pos cc <comma> nnp nns jj vb to vbn rb
vbz dt in nn))

;; THE END