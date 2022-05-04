#lang racket

(require data/queue)

(define (cop op)
  (lambda (a b k)
    (k (op a b))))

(define addC
  (lambda (a b k)
    ;; (k (+ a b))
    (addTask (lambda ()
               (k (+ a b))))
    ))

(define multC
  (lambda (a b k)
    ;; (k (* a b))
    (addTask (lambda ()
               (k (* a b))))
    ))

(define subC
  (lambda (a b k)
    ;; (k (- a b))
    (addTask (lambda ()
               (k (- a b))))
    ))

(define fact
  (lambda (n)
    (if (= n 1)
        1
        (* n (fact (- n 1))))))

(define factC
  (lambda (n k)
    (if (= n 1)
        (k 1)
        (subC n 1 (lambda (subRes)
                    (factC subRes (lambda (factN1)
                                    (multC n factN1 (lambda (multRes)
                                                      (k multRes))))))))))
;; (module+ test
;;   (factC 10 (lambda (x) (println x))))

(define fib
  (lambda (n)
    (if (<= n 1)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define fibC
  (lambda (n k)
    (if (<= n 1)
        (k n)
        (subC n 1
              (lambda (subRes)
                (fibC subRes
                      (lambda (fibn1)
                        (subC n 2
                              (lambda (subn2)
                                (fibC subn2
                                      (lambda (fibn2)
                                        (addC fibn1 fibn2
                                              (lambda (addRes)
                                                (k addRes))))))))))))))

;; (module+ test
;;   (fibC 8 (lambda (x) (println x))))

;; concurrency

(define tasks (make-queue))

(define (addTask t)
  (enqueue! tasks t))

(define (run)
  (let loop ()
    (if (queue-empty? tasks)
        'done
        (begin
          ;; (println (queue-length tasks))
          ((dequeue! tasks))
          (loop)))))

(fibC 8 (lambda (x) (println x)))
(factC 233 (lambda (x) (println x)))
(run)
