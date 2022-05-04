#lang racket

(require "mk.rkt")

(display-code #t)


(run* (x)
  (== 2 x))

(run* (x)
  (exist (y)
    (== y x)
    (== 3 y)))

(run* (x)
  (exist (y)
    (== y x)
    (== 3 y)
    (== 2 x)))

(run* (x)
  (exist (y)
    (== y x)
    (== 3 y)
    (== 2 5)))

(run* (x)
  (let ([y 2])
    (== y x)))

(run* (x)
  (let ([y x])
    (== 3 y)))

(run* (r)
  (let ([x 2])
    (== 2 x)))

(run* (l)
  (exist (x y)
    ; (== x y)
    (== 3 y)
    (== (cons x (cons y '())) l)))

(run* (r)
  (exist (x)
    (let ([y x])
      (exist (x)
        (== (list x y x) r)))))

(run* (x)
  (conde
    [(== 2 x)]
    [(== 5 x)]))

(run* (r)
  (exist (x y)
    (conde
      [(== 2 x) (== 3 y)]
      [(== 5 x) (== 7 y)])
    (== (list x y) r)))

(run* (x)
  (conde
    [(== 'virgin x)
     (== #t #f)]
    [(== 'olive x)]
    [(== 'oil x)]))

(run* (r)
  (exist (x y)
    (conde
      [(== 'a x) (== 'b y)]
      [(== 'c x) (== 'd y)])
    (== (list x y 'e) r)))

(run* (x)
  (== `(1 2 ,x 4) `(1 2 3 ,x)))

(run* (r)
  (exist (d)
    (== (cons r d) (cons 2 3))))

(define caro
  (lambda (p out)
    (exist (d)
      (== (cons out d) p))))

(run* (r)
  (caro '(2 3) r))

(define cdro
  (lambda (p out)
    (exist (a)
      (== (cons a out) p))))

(run* (r)
  (cdro '(2 3) r))

(define conso
  (lambda (a d out)
    (== (cons a d) out)))

(run* (r)
  (conso 2 3 r))

(define nullo
  (lambda (x)
    (== '() x)))

(define pairo
  (lambda (x)
    (exist (a d)
      (== (cons a d) x))))

(run* (r)
  (pairo (cons 2 3))
  (== 'olive r))

(define append
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else
       (cons (car ls1) (append (cdr ls1) ls2))])))


(define appendo
  (lambda (ls1 ls2 out)
    (conde
      [(nullo ls1)
       (== ls2 out)]
      [(exist (a d res)
         (== (cons a d) ls1)
         (conso a res out)
         (appendo d ls2 res))])))

(run* (r)
  (appendo '(1 2) '(3 4 5) r))

(run* (x)
  (appendo `(1 ,x) '(3 4 5) '(1 2 3 4 5)))

(run* (x)
  (appendo x '(3 4 5) '(1 2 3 4 5)))

(run* (x)
  (appendo '(1 2) x '(1 2 3 4 5)))

(run* (x)
  (appendo `(1 2) '(3 4 5) `(1 2 3 ,x 5)))


(define member
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(= x (car ls)) #t]
      [else
       (member x (cdr ls))])))

(member 2 '(1 2 3))
(member 2 '(1 4 3))


(define membero
  (lambda (x ls)
    (conde
      [(nullo ls) fail]
      [(caro ls x) succeed]
      [(exist (d)
         (cdro ls d)
         (membero x d))])))

(run* (r)
  (membero 2 '(1 2 3))
  (== r 5))

(run* (x)
  (membero x '(1 2 3)))

(run* (x)
  (membero x `(1 ,x 3)))

(run* (x)
  (membero 2 `(1 ,x 3)))

(run* (x)
  (exist (y)
    (membero y `(1 ,x 3))
    (membero 5 `(1 ,y 3))))

(run 10 (l)
  (membero 'tofu l))


(define rember
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(= x (car ls))
       (rember x (cdr ls))]
      [else
       (cons (car ls) (rember x (cdr ls)))])))


(rember 2 '(1 2 3 4 2 5))


(define rembero
  (lambda (x ls out)
    (conde
      [(nullo ls)
       (== '() out)]
      [(caro ls x)
       (exist (d)
         (cdro ls d)
         (rembero x d out))]
      [(exist (a d res)
         (conso a d ls)
         (conso a res out)
         (rembero x d res))])))

(run* (r)
  (rembero 2 '(1 2 3 4 2 5) r))

(run* (r)
  (exist (x y)
    (conde
      [(== 1 x)]
      [(== 2 x)]
      [(== 5 x)])
    (conde
      [(== 3 y)]
      [(== 4 y)])
    (== (list x y) r)))
