;; #lang racket

;; simple engine implementation with home-made shift/reset and timer

;; --------------- home-made shift/reset ---------------

;; This implementation of shift/reset is based on delimcc-simple.scm from:
;;   https://okmij.org/ftp/continuations/implementations.html

;; test by chez scheme
(define go #f)

;; list of stack fragments
(define pstack '())

(let ([v (call/cc
	  (lambda (k)
	    (set! go k)
	    (k #f)))])
  (when v
    (let* ([r (v)]
	   [h (car pstack)])
      (set! pstack (cdr pstack))
      (h r))))

(define do-reset
  (lambda (th)
    (call/cc
     (lambda (k)
       (set! pstack (cons k pstack))
       (go th)))))

(define do-shift
  (lambda (f)
    (call/cc
     (lambda (k)
       (go (lambda ()
	     (f (lambda (v)
	          (call/cc
                   (lambda (k1)
		     (set! pstack (cons k1 pstack))
		     (k v)))))))))))

(define-syntax reset
  (syntax-rules ()
    [(_ e0 e ...) (do-reset (lambda () e0 e ...))]))

(define-syntax shift
  (syntax-rules ()
    [(_ k e0 e ...) (do-shift (lambda (k) e0 e ...))]))


;; --------------- home-made timer ---------------

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

(define make-starter
  ;; make an engine starter from thunk
  (lambda (thunk)
    (lambda (ticks)
      (timer-interrupt-handler block)
      (set-timer ticks)
      (thunk))))

(define run-engine
  ;; may produce a continuation or a value
  (lambda (e ticks)
    (reset (e ticks))))

(define block
  ;; suspend engine and send continuation to exit of run-engine
  (lambda ()
    (set-timer (shift k k))))

(define engine
  ;; create an engine from starter or continuation
  (lambda (e)
    (lambda (ticks complete expire)
      (cond
       [(= ticks 0)
        (expire (engine e))]
       [else
        (let ([out (run-engine e ticks)])
          (cond
           [(procedure? out)              ; continuation
            (expire (engine out))]
           [else                          ; value
            (let ([ticks-left (set-timer 0)])
              ;; (printf "complete: ~a~n" complete)
              (complete ticks-left out))]))]))))

(define make-engine
  ;; top-level user interface
  (lambda (thunk)
    (engine (make-starter thunk))))

(define engine-block (lambda () (set-timer 0) (block)))
(define engine-return (lambda (args) (shift k args)))


;; --------------- tests ---------------

(define fib
  (lambda@t (n)
    (cond
     [(= n 0) 0]
     [(= n 1) 1]
     [else
      (+ (fib (- n 1)) (fib (- n 2)))])))

;; example usage of engine core directly
(define core1
  (make-starter
   (lambda ()
     (fib 12))))

(define get-value
  (lambda (e)
    (cond
     [(procedure? e)
      (get-value (run-engine e 100))]
     [else e])))

(get-value core1)
;; => 144


;; usual usage of engines

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

(e1 100
    (lambda (t v) (display (list t v)))
    (lambda (e)
      (set! e1 e)
      (display "expired")
      (newline)))
