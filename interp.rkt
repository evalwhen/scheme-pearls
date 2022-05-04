#lang racket

(struct Closure (param body env) #:transparent)

(define (literal? v)
  (or (number? v)
      (string? v)))

(define (lookup var env)
  (cdr (assoc var env)))

(define (extent var val env)
  (cons (cons var val) env))

(define (apply closure argument env)
  (interp (Closure-body closure)
          (extent (Closure-param closure)
                  argument
                  env)))

(define (getop name)
  (match name
    ['+ +]
    ['- +]
    ['* +]
    ['/ +]
    ['< +]
    ['> +]
    ['= +]))

(define empty-env
  `())

(define base-cont
  (lambda (v) v))

(define (interp exp env)
  (match exp
    [(? literal? n) n]
    [(? symbol? var) (lookup var env)]
    [`(lambda (,x) ,body) (Closure x body env)]
    [`(,f ,arg) (apply (interp f env) (interp arg env) env)]
    [`(,op ,a ,b) ((getop op) (interp a env) (interp b env))]
    [`(if ,a ,b ,c) (if (interp a env)
                        (interp b env)
                        (interp c env))]
    [_ (error (string-append "unkown exp" exp))]))

(module+ test
  (interp '322 '())
  (interp 'x '((x . 1)))
  (interp '(lambda (a) (+ a 3)) '((x . 1)))
  (interp '((lambda (a) (+ a 3)) 10) empty-env)
  (interp '(if (> 3 2) 3 2) empty-env))

(define (applyK closure argument env k)
  (interpC (Closure-body closure)
           (extent (Closure-param closure)
                   argument
                   env)
           k))

(define (interpC exp env k)
  (match exp
    [(? literal? n) (k n)]
    [(? symbol? var) (k (lookup var env))]
    [`(lambda (,x) ,body) (k (Closure x body env))]
    [`(,f ,arg) (interpC f env (lambda (fv)
                                 (interpC arg env (lambda (argv)
                                                    (applyK fv argv env k)))))]
    [`(,op ,a ,b) (interpC a env (lambda (v1)
                                   (interpC b env (lambda (v2)
                                                    (k ((getop op) v1 v2))))))]
    [`(if ,a ,b ,c) (interpC a env (lambda (av)
                                     (if av
                                         (interpC b env k)
                                         (interpC c env k))))]

    [_ (error (string-append "unkown exp" exp))]))

(module+ test
  (interpC '322 '() base-cont)
  (interpC 'x '((x . 1)) base-cont)
  (interpC '(lambda (a) (+ a 3)) '((x . 1)) base-cont)
  (interpC '((lambda (a) (+ a 3)) 10) empty-env base-cont)
  (interpC '(if (> 3 2) 3 2) empty-env base-cont))
