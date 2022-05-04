#lang racket
(require data/queue)
(require racket/control)
(require racket/engine)

(define show (lambda args
               (display (apply ~a
                               (append args (list "\n" ))
                               #:separator " "))))

(define queue (make-queue))

(define add-task
  (lambda (task)
    (let* ([task-until-sched
            (lambda (suspends) (reset (task suspends)))]
           [e1 (engine task-until-sched)])
      (enqueue! queue e1))))

;; (define add-task
;;   (lambda (task)
;;     (enqueue! queue (engine task))))



(define add-engine
  (lambda (engine)
    (enqueue! queue engine)))

(define next-engine
  (lambda () (dequeue! queue)))

(define ms 0.0001)
(define run
  (lambda ()
    (cond
      [(non-empty-queue? queue)
       (let* ([e1 (next-engine)])
         (cond [(not (engine-run ms e1))
                (add-engine e1)])
         (run))]
      [else (void)])))

(define sched (lambda () (shift task (add-task task))))

(define sum
  (lambda (n)
    (sched)
    (cond [(= n 0) 0]
          [else (+ n (sum (- n 1)))])))

(define sum-normal
  (lambda (n)
    (cond [(= n 0) 0]
          [else (+ n (sum-normal (- n 1)))])))
(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))
(define sum-no-sched
  (lambda (n)
    ; (sched)
    (cond [(= n 0) 0]
          [else (+ n (sum-no-sched (- n 1)))])))


(add-task (lambda (s) (show "(fib 40) =" (fib 40))))
(add-task (lambda (s) (show "(fib 30) =" (fib 30))))
(add-task (lambda (s) (show "(fib 31) =" (fib 31))))
(add-task (lambda (s) (show "(fib 5) =" (fib 5))))
;; (show "(sum 400) =" (sum 400))
;; (add-task (lambda (s) (show "(sum-no-sched 200000) =" (sum-no-sched 200000))))
;; (add-task (lambda (s) (show "(sum-no-sched 20000) =" (sum-no-sched 20000))))
;; (add-task (lambda (s) (show "(sum-no-sched 2000) =" (sum-no-sched 2000))))
;; (add-task (lambda (s) (show "(sum-no-sched 200) =" (sum-no-sched 200))))
;; (add-task (lambda (s) (show "(sum-no-sched 20) =" (sum-no-sched 20))))


(run)
