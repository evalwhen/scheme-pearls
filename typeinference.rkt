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



;;--------------------- substitution ----------------------

(struct var (name) #:transparent)

(define empty-s '())
(define size-s length)

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define walk
  (lambda (v s)
    (cond
      [(var? v)
       (let ([p (assq v s)])
         (cond
           [(not p) v]
           [else
            (walk (cdr p) s)]))]
      [else v])))

(define walk*
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v) v]
        [(pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s))]
        [else v]))))

(define unify
  (lambda (u v s)
    (let ([u (walk u s)]
          [v (walk v s)])
      (cond
        [(eq? u v) s]
        [(var? u) (ext-s u v s)]
        [(var? v) (ext-s v u s)]
        [(and (pair? u) (pair? v))
         (let ([s (unify (car u) (car v) s)])
           (and s (unify (cdr u) (cdr v) s)))]
        [(equal? u v) s]
        [else #f]))))

(define name-prefix "_")

(define set-name-prefix
  (lambda (s)
    (set! name-prefix s)))

(define name
  (lambda (n)
    (string->symbol
     (string-append name-prefix (number->string n)))))

(define reify-s
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v)
         (ext-s v (name (size-s s)) s)]
        [(pair? v)
         (reify-s (cdr v)
                  (reify-s (car v) s))]
        [else s]))))

(define reify
  (lambda (v s)
    (let ([v (walk* v s)])
      (walk* v (reify-s v empty-s)))))


;; -------- type checker --------

(define type-env0
  `((+ . (-> int int int))
    (- . (-> int int int))
    (* . (-> int int int))
    (/ . (-> int int int))
    (= . (-> int int bool))
    (< . (-> int int bool))
    (> . (-> int int bool))))

(define typecheck1
  (lambda (exp env s)
    (match exp
      [(? number? exp)
       (list 'int s)]
      [(? boolean? exp)
       (list 'bool s)]
      [(? symbol? exp)
       (let ([p (assq exp env)])
         (cond
           [(not p)
            (error "Unbound variable: " exp)]
           [else
            (list (cdr p) s)]))]
      [`(lambda (,params : ,in-types) ... : ,out-type
          ,body)
       (let* ([in-types1 (map (lambda (t) (if (eq? t '?) (var '?) t))
                              in-types)]
              [out-type1 (if (eq? out-type '?) (var '?) out-type)]
              [env1 (ext-env* params in-types1 env)])
         (match-let ([`(,actual ,s1) (typecheck1 body env1 s)])
           (let ([s2 (unify actual out-type1 s1)])
           (cond
             [s2
              (list `(-> ,@in-types1 ,out-type1) s2)]
             [else
              (error "expected output type: " out-type1 ", but got: " actual)]))))]
      [`(,op ,args ...)
       (match-let* ([`(,op-type ,s1) (typecheck1 op env s)]
                    [`(,arg-types ,s2) (check-args args env s1)])
         (match op-type
           [`(-> ,in-types ... ,out-type)
            (let ([s3 (unify arg-types in-types s2)])
              (cond
                [s3
                 (list out-type s3)]
                [else
                 (error "expected input type: "
                        (reify in-types s2)
                        ", but got: "
                        (reify arg-types s2))]))]
           [_
            (error "Calling non-function: " op)]))])))

(define check-args
  (lambda (args env s)
    (cond
      [(null? args)
       (list '() s)]
      [else
       (match-let ([`(,t1 ,s1) (typecheck1 (car args) env s)])
         (match-let ([`(,ts ,s2) (check-args (cdr args) env s1)])
           (list (cons t1 ts) s2)))])))

(define typecheck
  (lambda (exp)
    (match-let ([`(,tp ,s1) (typecheck1 exp type-env0 empty-s)])
      (reify tp s1))))

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

(typecheck '(lambda (x : ?) : ?
              (* x x)))

(typecheck '((lambda (x : ?) : ?
              (* x x))
             3))

(typecheck '(lambda (x : ?) (y : ?) : ?
              (+ 1 (* x y))))

(typecheck '(lambda (x : ?) (y : ?) : ?
              (< 1 (* x y))))
