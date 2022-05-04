#lang racket

(require data/queue)
(require racket/control)
(require racket/engine)

(define show
  (lambda args
    (display (apply ~a
                    (append args (list "\n" ))
                    #:separator " "))))

(define tasks (make-queue))

(define addTask
  (lambda (t)
    (enqueue! tasks t)))

(define engine-loop
  (lambda ()
    (if (queue-empty? tasks)
        (void)
        (begin (reset ((dequeue! tasks)))
               (engine-loop)))))

(define eng-obj (engine (lambda (s) (engine-loop))))

(define run
  (lambda ()
    (let loop ([res (engine-run 1 eng-obj)])
      (if res
          (void)
          (loop (engine-run 1 eng-obj))))))

(define sched
  (lambda ()
    (shift k (addTask k))))

(define sum-with-sched
  (lambda (n)
    (sched)
    (if (= n 0)
        0
        (+ n (sum-with-sched (- n 1))))))

(define sum
  (lambda (n)
    (if (= n 0)
        0
        (+ n (sum (- n 1))))))

(addTask (lambda () (show "(sum 20) =" (sum 20))))
(addTask (lambda () (show "(sum-with-sched 7) =" (sum 7))))
(addTask (lambda () (show "(sum-with-sched 3) =" (sum 3))))

(run)
