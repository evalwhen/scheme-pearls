;; simple engine implementation with home-made timer

;; --------------- home-made timer implementation ---------------

;; To use Chez Scheme's system timer, just comment out this section
;; and uncomment the other definition of 'lambda@t'

(define handler #f)
(define clock 0)

(define timer-interrupt-handler
  (lambda (f)
    (set! handler f)))

(define set-timer
  (lambda (ticks)
    (let ([time-left clock])
      (set! clock ticks)
      time-left)))

(define decrement-timer
  (lambda ()
    (cond
     [(= clock 0) (handler)]
     [else
      (set! clock (- clock 1))])))

(define-syntax lambda@t
  (syntax-rules ()
    [(_ args e0 e1 ...)
     (lambda args (decrement-timer) e0 e1 ...)]))

;; (define-syntax lambda@t
;;   (syntax-rules ()
;;     [(_ args e0 e1 ...)
;;      (lambda args e0 e1 ...)]))

(define-syntax atomic
  (syntax-rules ()
    [(_ e0 e ...)
     (let ([rest (set-timer 0)])
       (let ([value (begin e0 e ...)])
         (set-timer rest)
         value))]))

(define show
  (lambda args
    (atomic
     (for-each display args)
     (newline))))


;; --------------- engine ---------------

(define exit #f)

(define eng
  (lambda (k)
    (lambda (ticks complete expire)
      (cond
       [(= ticks 0)
        (expire (eng k))]
       [else
        (let ([kv (run-engine k ticks)])
          (cond
           [(procedure? kv)             ; continuation
            (expire (eng kv))]
           [else                        ; value
            (let ([rest (set-timer 0)])
              (complete rest kv))]))]))))

(define make-engine
  (lambda (thunk)
    (eng (lambda (ticks)
           (set-timer ticks)
           (exit (thunk))))))

(define run-engine
  (lambda (thunk ticks)
    (call/cc
     (lambda (k)
       (set! exit k)
       (set-timer 0)
       (timer-interrupt-handler block)
       (thunk ticks)))))

(define block
  (lambda ()
    (set-timer (call/cc (lambda (k) (exit k))))))

(define engine-return (lambda (args) (exit args)))
(define engine-block (lambda () (set-timer 0) (block)))


;; --------------- tests ---------------

(define fib
  (lambda@t (n)
    (cond
     [(= n 0) 0]
     [(= n 1) 1]
     [else
      (+ (fib (- n 1)) (fib (- n 2)))])))

(define e1
  (make-engine
   (lambda ()
     (fib 12))))


(e1 100
    (lambda (t v) (display (list t v)))
    (lambda (e)
      (set! e1 e)
      (display "expired")
      (newline)))

(e1 100
    (lambda (t v) (display (list t v)))
    (lambda (e)
      (set! e1 e)
      (display "expired")
      (newline)))

(e1 100
    (lambda (t v) (display (list t v)))
    (lambda (e)
      (set! e1 e)
      (display "expired")
      (newline)))

(e1 100
    (lambda (t v) (display (list t v)))
    (lambda (e)
      (set! e1 e)
      (display "expired")
      (newline)))

(e1 100
    (lambda (t v) (display (list t v)))
    (lambda (e)
      (set! e1 e)
      (display "expired")
      (newline)))
