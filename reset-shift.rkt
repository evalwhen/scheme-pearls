#lang racket

(require racket/control)

(define-syntax-rule (break) (control k '()))
(define-syntax-rule (while cond body ...)  
  (prompt
   (let loop ()
     (when cond
       body ...
       (loop)))))

(define i 1)
(while (< i 3) (writeln "Hi") (break) (set! i (+ i 1)))

(reset
  (+ 1 (shift k (let ([x (k 1)] [y (k 2)])
                     (k (* x y))))))


(define (leak-test1 identity-thunk)
  (let loop ((id (lambda (x) x)))
    (loop (id (identity-thunk)))))

(leak-test1 (lambda () (reset (shift k k))))
