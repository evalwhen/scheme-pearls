#lang racket

(require racket/control)

(define empty-env '())

(define ext-env
  (lambda (name value old-env)
    (cons (cons name value) old-env)))

(define op-map
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

(define id (lambda (x) x))

(define interp1
  (lambda (exp env)
    (match exp
      [(? number? exp)
       exp]
      [(? symbol? exp)
       (let ([p (assq exp env)])
         (cond
           [(not p)
            (error "Unbound variable: " exp)]
           [else
            (cdr p)]))]
      [`(lambda (,param) ,body)
       `(closure ,exp ,env)]
      [`(,op ,e1 ,e2)
       (let ([p (assq op op-map)])
         (cond
           [(not p)
            (error "Unsupported op: " op)]
           [else
            (let ([op-fun (cdr p)])
              (op-fun (interp1 e1 env) (interp1 e2 env)))]))]
      [`(,op ,arg)
       (let ([op-value (interp1 op env)]
             [arg-value (interp1 arg env)])
         (match op-value
           [`(closure (lambda (,param) ,body) ,closure-env)
            (let ([new-env (ext-env param arg-value closure-env)])
              (interp1 body new-env))]
           [_
            (error "Calling non-function: " op)]))])))

(define interp
  (lambda (exp)
    (interp1 exp '())))


(define interp-cps1
  (lambda (exp env k)
    (match exp
      [(? number? exp)
       (k exp)]

      [(? symbol? exp)
       (let ([p (assq exp env)])
         (cond
           [(not p)
            (error "Unbound variable: " exp)]
           [else
            (k (cdr p))]))]
      [`(lambda (,param) ,body)
       (k `(closure ,exp ,env))]

      [`(call/cc (lambda (,param) ,body))
       (let ([new-env (ext-env param `(callcc-k ,k) env)])
         (interp-cps1 body new-env k))]

      [`(shift ,param ,body)
       (let ([new-env (ext-env param `(shift-k ,k) env)])
         (interp-cps1 body new-env id))]

      ;; eval the body of reset at a new evaluation context
      [`(reset ,e)
       (k (interp-cps1 e env id))]

      [`(,op ,e1 ,e2)
       (let ([p (assq op op-map)])
         (cond
           [(not p)
            (error "Unsupported op: " op)]
           [else
            (let ([op-fun (cdr p)])
              ; (op-fun (interp e1 env) (interp e2 env))
              (interp-cps1 e1 env
                      (lambda (v1)
                        (interp-cps1 e2 env
                                (lambda (v2)
                                  (k (op-fun v1 v2)))))))]))]

      [`(,op ,arg)
       (interp-cps1 op env
               (lambda (op-value)
                 (interp-cps1 arg env
                         (lambda (arg-value)
                           (match op-value
                             [`(closure (lambda (,param) ,body) ,closure-env)
                               (let ([new-env (ext-env param arg-value closure-env)])
                                 (interp-cps1 body new-env k))]
                             [`(callcc-k ,ck)
                              (ck arg-value)]
                             [`(shift-k ,sk)
                              (k (sk arg-value))]
                             [_
                              (error "Calling non-function: " op)])))))])))


(define interp-cps
  (lambda (exp)
    (interp-cps1 exp '() (lambda (x) x))))


(interp-cps '(+ 1 (* 2 3)))
;; => 7

(interp-cps '(((lambda (x) (lambda (y) (+ x y))) 2) 3))
;; => 5

(interp-cps '(+ 1 (call/cc (lambda (k) (+ 10 (k 5))))))
;; => 6

(interp-cps '(+ 1 (reset (+ 1 (+ 1 (shift k (k 10)))))))
;; => 13

(interp-cps '(+ 1 (reset (+ 1 (+ 1 (shift k (+ (k 0) (k 0))))))))
;; => 5

(interp-cps '(+ 1 (reset (+ 1 (+ 1 (shift k 10))))))
;; => 11


;; Scheme call/cc examples

(+ 1 (call/cc (lambda (k) (+ 10 (k 5)))))
;; => 6
(+ 1 (call/cc (lambda (k) (+ 10 5))))
;; => 16


(define cc1 (lambda (x) x))

(+ 1 (call/cc (lambda (k) (set! cc1 k) (+ 10 (k 5)))))
;; => 6

(cc1 6)
;; => 7


(define fact
  (lambda (n)
    (cond
      [(= n 0) 1]
      [else
       (* n (fact (- n 1)))])))


(define cc2 (lambda (x) x))

(let ([x (fact 5)]
      [y (call/cc (lambda (k) (set! cc2 k) (k (* 2 3))))])
  (+ x y))
;; => 126

(cc2 100)
;; => 220
