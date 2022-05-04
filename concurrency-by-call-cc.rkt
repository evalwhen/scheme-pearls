#lang racket

(require data/queue)
(require racket/control)

(define tasks (make-queue))

(define (addTask t) (enqueue! tasks t))

(define run
  (lambda ()
    (if (queue-empty? tasks)
        (void)
        (begin (reset ((dequeue! tasks)))
               (run)))))

(define sched
  (lambda ()
    (shift k (addTask k))))

(define sum
  (lambda (n)
    (sched)
    ;; (println "here")
    (if (= n 0)
        0
        (+ n (sum (- n 1))))))


(addTask (lambda () (println (sum 7))))
(addTask (lambda () (println (sum 3))))
(addTask (lambda () (println (sum 5))))

(run)
