#lang racket

(require racket/hash)


;; -------------------- interpreter --------------------

(define env0
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (< . ,<)
    (> . ,>)
    (= . ,=)
    (exp . ,exp)
    (expt . ,expt)))

(define ext-env
  (lambda (name value old-env)
    (cons (cons name value) old-env)))

;; extend environment for multiple arguments
(define ext-env*
  (lambda (names values env)
    (cond
      [(not (= (length names) (length values)))
       (error "number of names and values don't match")]
      [(null? names) env]
      [else
       (ext-env* (cdr names)
                 (cdr values)
                 (ext-env (car names) (car values) env))])))

(define inputs (make-hasheq))

(define push-inputs
  (lambda (exp value)
    (let ([found (hash-ref inputs exp #f)])
      (if found
          (hash-set! inputs exp (cons value found))
          (hash-set! inputs exp (list value))))))

(define pop-inputs
  (lambda (exp)
    (let ([found (hash-ref inputs exp #f)])
      (cond
        [found
         (cond
           [(null? (cdr found))
            (hash-remove! inputs exp)
            (car found)]
           [else
            (let ([result (car found)])
              (hash-set! inputs exp (cdr found))
              result)])]
        [else #f]))))

(define interp1
  (lambda (exp env)
    (match exp
      [(? number? exp)
       exp]
      [(? symbol? exp)
       (let ([p (assq exp env)])
         (cond
           [(not p)
            (error "unbound variable: " exp)]
           [else
            (cdr p)]))]
      [`(lambda (,params ...) ,body)
       `(closure ,exp ,env)]
      [`(let ([,x* ,e*] ...) ,body)
       (let ([v* (map (lambda (a) (interp1 a env)) e*)])
         ;; save inputs into hashtable
         ;; (push-inputs exp v*)
         (let ([env1 (ext-env* x* v* env)])
           (interp1 body env1)))]
      [`(if ,test ,e1 ,e2)
       (let ([tv (interp1 test env)])
         (push-inputs exp tv)
         (if tv
             (interp1 e1 env)
             (interp1 e2 env)))]
      [`(,op ,args ...)
       (let ([op-value (interp1 op env)]
             [arg-values (map (lambda (a) (interp1 a env)) args)])
         ;; save inputs into hashtable
         (push-inputs op op-value)
         (for-each push-inputs args arg-values)
         (match op-value
           [(? procedure? f)
            (apply f arg-values)]
           [`(closure (lambda (,params ...) ,body) ,closure-env)
            (let ([env1 (ext-env* params arg-values closure-env)])
              (interp1 body env1))]
           [_
            (error "Calling non-function: " op)]))])))

(define interp
  (lambda (exp)
    (interp1 exp env0)))


;; -------------------- differentiate --------------------

(define +back
  (lambda (inputs out)
    (list out out)))

(define -back
  (lambda (inputs out)
    (list out (- out))))

(define *back
  (lambda (inputs out)
    (match inputs
      [`(,x ,y)
       (list (* out y) (* out x))])))

(define /back
  (lambda (inputs out)
    (match inputs
      [`(,x ,y)
       (list (/ out y)
             (* out (/ (- x) (* y y))))])))

(define exp-back
  (lambda (inputs out)
    (match inputs
      [`(,x)
       (list (* out (exp x)))])))

(define expt-back
  (lambda (inputs out)
    (match inputs
      [`(,x ,y)
       (list (* out (expt x (- y 1)) y)
             (* out (expt x y) (log x)))])))

(define env-back
  `((,+ . ,+back)
    (,- . ,-back)
    (,* . ,*back)
    (,/ . ,/back)
    (,exp . ,exp-back)
    (,expt . ,expt-back)))

;; merge key-value with any existing key in hashtable env
(define merge
  (lambda (key value env)
    (let* ([old (hash-ref env key 0)]
           [new (+ old value)])
      (hash-set! env key new)
      new)))

;; map lists from right to left
(define rmap
  (lambda (f . lists)
    (reverse (apply map f (map reverse lists)))))

;; go backwards and compute gradient of exp
(define diff
  (lambda (exp env out)
    (match exp
      [(? number? exp)
       out]
      [(? symbol? exp)
       (merge exp out env)]
      [`(lambda (,params ...) ,body)
       0]
      [`(let ([,x* ,e*] ...) ,body)
       (let ([env1 (make-hasheq)])
         (diff body env1 out)
         (let ([grad-params (map (lambda (p) (hash-ref env1 p 0)) x*)])
           (for-each (lambda (p) (hash-remove! env1 p)) x*)
           (hash-for-each env1 (lambda (k v) (merge k v env)))
           ;; return gradient of args
           (rmap (lambda (a d) (diff a env d)) e* grad-params)))]
      [`(if ,test ,e1 ,e2)
       (let ([saved (pop-inputs exp)])
         (if saved
             (diff e1 env out)
             (diff e2 env out)))]
      [`(,op ,args ...)
       (let ([input (rmap pop-inputs args)]
             [op-value (pop-inputs op)])
         (let ([p (assq op-value env-back)])
           (cond
             [p
              ;; backward function is defined
              (let* ([back (cdr p)]
                     [grad-input (back input out)])
                ;; return gradient of args
                (rmap (lambda (a d) (diff a env d)) args grad-input))]
             [else
              (match op-value
                [`(closure (lambda (,params ...) ,body) ,closure-env)
                 (let ([env1 (make-hasheq)])
                   (diff body env1 out)
                   (let ([grad-params (map (lambda (p) (hash-ref env1 p 0)) params)])
                     (for-each (lambda (p) (hash-remove! env1 p)) params)
                     (hash-for-each env1 (lambda (k v) (merge k v env)))
                     ;; return gradient of args
                     (rmap (lambda (a d) (diff a env d)) args grad-params)))]
                [_
                 (error "unexpected op-value: " op-value)])])))])))


;; -------------------- test --------------------

(define approx-equal?
  (lambda (x y eps)
    (cond
      [(and (null? x) (null? y))
       #t]
      [(and (pair? x) (pair? y))
       (and (approx-equal? (car x) (car y) eps)
            (approx-equal? (cdr x) (cdr y) eps))]
      [(and (number? x) (number? y))
       (< (abs (- x y)) eps)]
      [else
       (eqv? x y)])))

(define demo
  (lambda (exp)
    (display "------------------------------------\n")
    (printf "~a~n" exp)
    (define env0 (make-hasheq))
    (printf "value: ~a~n" (interp exp))
    (printf "gradient: ~a~n" (diff exp env0 1))))

(define test
  (lambda (exp expected)
    (display "------------------------------------\n")
    (printf "~a~n" exp)
    (define env0 (make-hasheq))
    (printf "value: ~a~n" (interp exp))
    ;; (printf "inputs: ~a~n" inputs)
    (let ([grad (diff exp env0 1)])
      (printf "gradient: ~a~n" grad)
      (if (approx-equal? grad expected 0.00001)
          (printf "[pass]~n")
          (error "[fail] expected: " expected ", but got: " grad)))))


(test '(+ 2 3)
      '(1 1))

(test '((lambda (x y) (+ x y)) 2 3)
      '(1 1))

(test '(- 2 3)
      '(1 -1))

(test '((lambda (x y) (- x y)) 2 3)
      '(1 -1))

(test '(* 2 3)
      '(3 2))

(test '((lambda (x y) (* x y)) 2 3)
      '(3 2))

(test '((lambda (u v) ((lambda (x y) (* x y)) u v)) 2 3)
      '(3 2))

(test '(/ 2 3)
      '(1/3 -2/9))

(test '((lambda (x y) (/ x y)) 2 3)
      '(1/3 -2/9))

(test '((lambda (x) (exp x)) 2)
      '(7.38905609893065))

(test '((lambda (x) (/ 1 (exp x))) 2)
      '(-0.13533528323661267))

(test '((lambda (x y) (* (exp x) y)) 2 3)
      '(22.16716829679195 7.38905609893065))

(test '((lambda (x y) (expt x y)) 2 3)
      '(12 5.545177444479562))

(test '((lambda (x) (expt (exp 1) x)) 2)
      '(7.3890560989306495))

(test '((lambda (x) (* x (* x x))) 2)
      '(12))

(test '((lambda (x) (* (* x x) (* x x))) 2)
      '(32))

(test '((lambda (x) (* x (* x (* x x)))) 2)
      '(32))

(test '((lambda (x y) (* (* x x) (* y y))) 2 3)
      '(36 24))

(test '((lambda (x y) (* (* x y) (* y x))) 2 3)
      '(36 24))

(test '((lambda (x y) (* x (* y (* y x)))) 2 3)
      '(36 24))

(test '((lambda (x y) (+ (* x x) (* y y))) 2 3)
      '(4 6))

(test '((lambda (x y) (/ (* x x) (* y y))) 2 3)
      '(4/9 -8/27))

(test '((lambda (x y) (/ (* x x) (+ x (* y y)))) 2 3)
      '(40/121 -24/121))

(test '((lambda (x y z) (+ (* x y) z)) 2 3 4)
      '(3 2 1))

(test '((lambda (x y z) (+ (* x y) x)) 2 3 4)
      '(4 2 0))

(test '((lambda (x y z) (* (+ x y) z)) 2 3 4)
      '(4 4 5))

(test '((lambda (x) ((lambda (y) (* y 2)) x)) 3)
      '(2))

(test '((lambda (x) ((lambda (x) (* x 2)) x)) 3)
      '(2))

(test '(((lambda (x) (lambda (y) (* x y))) 2) 3)
      '(2))

(test '((lambda (x y) (* x y)) 2 3)
      '(3 2))

(test '((lambda (x) (* x 2)) ((lambda (x y) (* x y)) 2 3))
      '((6 4)))

;; must use make-hasheq
(test '((lambda (x y) (* x y)) ((lambda (x y) (* x y)) 2 3)
                               ((lambda (x y) (+ x y)) 2 3))
      '((15 10) (6 6)))

(test '((lambda (f) (f 3)) (lambda (x) (* x 2)))
      '(0))
;; why?

(test '((lambda (x) (* x ((lambda (f) (f 3)) (lambda (x) (* x 2))))) 5)
      '(6))

(test '((lambda (y) ((lambda (f) (f y)) (lambda (x) (* x 2)))) 3)
      '(2))

;; four
(test '(((lambda (f) (lambda (x) (f (f (f x)))))
         (lambda (x) (* x x)))
        2)
      '(1024))

(test '((lambda (x) ((lambda (f) (f (f (f x))))
                     (lambda (x) (* x x)))) 2)
      '(1024))

(test '(((lambda (f) (lambda (x) (+ (f x) (f (f x)))))
         (lambda (x) (* x x)))
        2)
      '(36))

(test '(((lambda (f) (lambda (x) (+ (f (f x)) (+ (f (f (f (f x)))) (f x)))))
         (lambda (x) (* x x)))
        2)
      '(524324))

(test '((lambda (x)
          ((lambda (times)
             (times x (times 2 x)))
           (lambda (a b) (* a b)))) 2)
      '(8))

(test '((lambda (x)
          ((lambda (times)
             (times (times 2 x) (times 3 x)))
           (lambda (a b) (* a b)))) 2)
      '(24))

(test '(if (< 0 1)
           (* 2 3)
           (+ 2 3))
      '(3 2))

(test '(if (< 1 0)
           (* 2 3)
           (+ 2 3))
      '(1 1))

(test '(let ([x 2]
             [y 3])
         (* x y))
      '(3 2))

(test '(let ([f (lambda (x) (* x x))]
             [x 3])
         (f x))
      '(0 6))

(test '(let ([f (lambda (x) (* x x))]
             [g (lambda (x) (* x 2))]
             [x 3])
         (g (f x)))
      '(0 0 12))

(test '((lambda (times a b) ((times a) b))
        (lambda (x) (lambda (y) (* x y)))
        2 3)
      '(0 3 2))

(test '((lambda (times a b c) ((times (* a b)) c))
        (lambda (x) (lambda (y) (* x y)))
        2 3 4)
      '(0 12 8 6))


;; base case is noncontinuous
#;(test '(let ([Y (lambda (f)
                    ((lambda (x) (f (lambda (v) ((x x) v))))
                     (lambda (x) (f (lambda (v) ((x x) v))))))]
               [times (lambda (f)
                        (lambda (x)
                          (lambda (y)
                            (if (= y 0)
                                x
                                (+ x ((f x) (- y 1)))))))]
               [a 2]
               [b 10])
           (((Y times) a) b))
        '(0 0 1 0))

#;(test '((lambda (Y times a b)
            (((Y times) a) b))
          (lambda (f)
            ((lambda (x) (f (lambda (v) ((x x) v))))
             (lambda (x) (f (lambda (v) ((x x) v))))))
          (lambda (f)
            (lambda (x)
              (lambda (y)
                (if (= y 1)
                    x
                    (+ x ((f x) (- y 1)))))))
          2 3)
        '(0 0 1 0))
