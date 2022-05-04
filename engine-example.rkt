#lang racket

(require racket/engine)

(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define fib-iter
  (lambda (n f0 f1)
    (if (= n 0)
        f1
        (fib-iter (- n 1) f1 (+ f0 f1)))))

(define sum-no-sched
  (lambda (n)
    ; (sched)
    (cond [(= n 0) 0]
          [else (+ n (sum-no-sched (- n 1)))])))

(define test-eng (engine (lambda (s) (s false) (println (sum-no-sched 2000)))))

(define run
  (lambda ()
    (let loop ([res (engine-run 0.0001 test-eng)] [times 1])
      (if res
          times
          (loop (engine-run 0.0001 test-eng) (+ times 1))))))

(run)
