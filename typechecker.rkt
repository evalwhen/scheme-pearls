#lang racket


(define ext-env
  (lambda (name value env)
    (cons (cons name value) env)))

(define ext-env*
  (lambda (names values env)
    (cond
      [(and (null? names) (null? values))
       env]
      [(and (pair? names) (pair? values))
       (ext-env*
        (cdr names)
        (cdr values)
        (ext-env (car names) (car values) env))]
      [else
       (error "number of names and values don't match")])))

(define env0
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

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
      [`(lambda (,params ...) ,body)
       `(closure ,exp ,env)]
      [`(,op ,args ...)
       (let ([op-value (interp1 op env)]
             [arg-values (map (lambda (a) (interp1 a env)) args)])
         (match op-value
           [(? procedure? proc)
            (apply proc arg-values)]
           [`(closure (lambda (,params ...) ,body) ,closure-env)
            (let ([new-env (ext-env* params arg-values closure-env)])
              (interp1 body new-env))]
           [_
            (error "Calling non-function: " op)]))])))

(define interp
  (lambda (exp)
    (interp1 exp env0)))


;; -------- tests --------

(interp '(+ 1 (* 2 3)))

(interp '((lambda (x) (* x x)) 3))

(interp '((lambda (x y) (* x y)) 3 4))



;; -------- type checker --------

(define type-env0
  `((+ . (-> int int int))
    (- . (-> int int int))
    (* . (-> int int int))
    (/ . (-> int int int))))

(define typecheck1
  (lambda (exp env)
    (match exp
      [(? number? exp)
       'int]
      [(? boolean? exp)
       'bool]
      [(? symbol? exp)
       (let ([p (assq exp env)])
         (cond
           [(not p)
            (error "Unbound variable: " exp)]
           [else
            (cdr p)]))]
      [`(lambda (,params : ,in-types) ... : ,out-type
          ,body)
       (let* ([env1 (ext-env* params in-types env)]
              [actual (typecheck1 body env1)])
         (cond
           [(equal? actual out-type)
            `(-> ,@in-types ,out-type)]
           [else
            (error "expected output type: " out-type ", but got: " actual)]))]
      [`(,op ,args ...)
       (let ([op-type (typecheck1 op env)]
             [arg-types (map (lambda (a) (typecheck1 a env)) args)])
         (match op-type
           [`(-> ,in-types ... ,out-type)
            (cond
              [(equal? arg-types in-types)
               out-type]
              [else
               (error "expected input type: " in-types ", but got: " arg-types)])]
           [_
            (error "Calling non-function: " op)]))])))

(define typecheck
  (lambda (exp)
    (typecheck1 exp type-env0)))


(typecheck 2)
(typecheck #f)

(typecheck '(lambda (x : int) : int
              x))

(typecheck '((lambda (x : int) : int
               (* x x)) 3))

(typecheck '(lambda (x : int) (y : int) : int
              (+ 1 (* x y))))

(typecheck '((lambda (x : int) (y : int) : int
               (+ 1 (* x y)))
             2 3))


;(typecheck '(lambda (n : nat) : (vec atom n)
;              ...))
;
;(typecheck '(lambda (n : nat) (v : (vec atom n)) (index : nat) : atom
;              ...))
;
