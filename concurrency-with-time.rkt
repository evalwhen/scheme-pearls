#lang racket

(require data/queue)
(require racket/control)
(require racket/engine)

(define show
  (lambda args
    (display (apply ~a
                    (append args (list "\n" ))
                    #:separator " "))))

;; ms
(define TIME-SLICE 0.000000000001)
(define engine-tasks (make-queue))
(define sched-tasks (make-queue))


(define add-task
  (lambda (t type)
    (if (eq? type 'engine)
        (enqueue! engine-tasks (engine (lambda (s) (t))))
        (enqueue! sched-tasks t))))

(define sched-loop
  (lambda ()
    (if (queue-empty? sched-tasks)
        (void)
        (begin (reset ((dequeue! sched-tasks)))
               (sched-loop)))))

(add-task sched-loop 'engine)

(define sched
  (lambda ()
    (shift k (add-task k 'yield))))

(define run
  (lambda ()
    (if (queue-empty? engine-tasks)
        (void)
        (let ([t (dequeue! engine-tasks)])
          ;; is task finished?
          (if (engine-run TIME-SLICE t)
              (void)
              (enqueue! engine-tasks t))
          (run)))))

;; use case
(define sum-sched
  (lambda (n)
    (sched)
    (if (= n 0)
        0
        (+ n (sum-sched (- n 1))))))

(define fib-sched
  (lambda (n)
    (sched)
    (if (< n 2)
        n
        (+ (fib-sched (- n 1))
           (fib-sched (- n 2))))))

(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))
(define fib-iter
  (lambda (n)
    (letrec ([F (lambda (n f0 f1)
                  (if (= n 0)
                      f1
                      (F (- n 1) f1 (+ f0 f1))))])
      (F n 0 1))))

(define sum-no-sched
  (lambda (n)
    ; (sched)
    (cond [(= n 0) 0]
          [else (+ n (sum-no-sched (- n 1)))])))

;; (add-task (lambda () (show "(fib 31) = " (fib 31))) 'engine)

;; (add-task (lambda () (show "(fib 30) = " (fib 30))) 'engine)
;; (add-task (lambda () (show "(fib 5) = " (fib 5))) 'engine)

(add-task (lambda () (show "(sum-no-sched 200000) =" (sum-no-sched 200000))) 'engine)
(add-task (lambda () (show "(sum-no-sched 20000) =" (sum-no-sched 20000))) 'engine)
(add-task (lambda () (show "(sum-no-sched 2000) =" (sum-no-sched 2000))) 'engine)
(add-task (lambda () (show "(sum-no-sched 200) =" (sum-no-sched 200))) 'engine)
(add-task (lambda () (show "(sum-no-sched 20) =" (sum-no-sched 20))) 'engine)



(run)
