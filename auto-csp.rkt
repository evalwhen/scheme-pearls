#lang racket

(define id (lambda (x) x))

(define serial 0)

(define gensym
  (lambda (base)
    (set! serial (+ 1 serial))
    (string->symbol
     (string-append (symbol->string base)
                    (number->string serial)))))
(define binop?
  (lambda (sym)
    (member sym '(+ - * / > < =))))

(define cpser1
  (lambda (exp ctx)
    (match exp
      [(? symbol? exp)
       (ctx exp)]
      [(? number? exp)
       (ctx exp)]
      [`(,(? binop? op) ,a ,b)
       (cpser1 a (lambda (e1)
                   (cpser1 b (lambda (e2)
                               (ctx `(,op ,e1 ,e2))))))]
      [`(if ,c ,t ,f)
       (cpser1 c (lambda (ce)
                   `(if ,ce ,(cpser1 t ctx) ,(cpser1 f ctx))))]
      [`(lambda (,param) ,body)
       (ctx `(lambda (,param k)
               ,(cpser1 body (lambda (x) `(k ,x)))))]
      [`(let ([,x ,e]) ,body)
       (cpser1 `((lambda (,x) ,body) ,e) ctx)]
      [`(define ,n ,e)
       `(define ,n ,(cpser1 e ctx))]
      [`(,op ,arg)
       (cpser1 op (lambda (e1)
                    (cpser1 arg (lambda (e2)
                                  (let ([name (gensym 'v)])
                                    `(,e1 ,e2 (lambda (,name) ,(ctx name))))))))])))

(define cpser
  (lambda (exp)
    (set! serial 0)
    (cpser1 exp id)))

(cpser '(lambda (x) (f x)))
(cpser '(lambda (x) (f (g x))))
(cpser '(lambda (x) ((f x) (g y))))
(cpser '((lambda (x) (f x)) y))

(cpser '(lambda (x) (+ 1 2)))
(cpser '(lambda (x) (- 1 2)))

(cpser '(lambda (x) (if (> x 1) (f x) (f g))))
(cpser '(lambda (x) (if (p c) (f x) (f g))))
(cpser '(let ([x (f g)]) (x c)))

(cpser '(define (fact)
          (if (= n 1)
              1
              (* n (fact (- n 1))))))
