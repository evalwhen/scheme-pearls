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
