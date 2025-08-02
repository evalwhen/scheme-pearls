;; 实现一下论文中的非确定性计算
;; https://okmij.org/ftp/tagless-final/nondet/nondet-paper.pdf
;;; ======================
;;; 修正后的非确定性 DSL 实现
;;; ======================

(define (nd-nil) '(()))
(define (nd-fail) '())  ; 失败：无选择

(define (nd-choice a b) ; 非确定性选择：合并选择集
  (append a b))

(define (nd-list lst)   ; 确定列表 -> 单选择的非确定性列表
  (list lst))

(define (nd-cons x choices)  ; 向所有选择添加元素
  (map (lambda (choice) (cons x choice)) choices))

;; 辅助函数：连接映射 (concatMap)
(define (concat-map f lst)
  (apply append (map f lst)))

;; 递归原语 recur (修正版)
;; 把 nd-nil 看作 nil, 以下 nd-recur 的实现满足等式组1:
;; recur c n nil        ≡ n
;; recur c n (cons h t) ≡ c (h,t) (fun () -> recur c n t)
;; 以下为论证过程
;; (nd-recur f n nd-nil) = z

;; (nd-recur f n nd-lst) =
;; (nd-recur f n (nd-list '(1 2))) =
;; (nd-recur f n '((1 2))) =
;; (f 1 (2) (lambda () (nd-recur f z '((2)))))
(define (nd-recur f z nd-lst)
  (concat-map
   (lambda (choice)
     (if (null? choice)
         z  ; 空列表返回基值
         (f (car choice)
            (cdr choice)
            (lambda () (nd-recur f z (list (cdr choice)))))))
   nd-lst))

;;
(define (nd-recur-simple f z lst)
  (if (null? lst)
      z  ; 空列表返回基值
      (let ((h (car lst))
            (t (cdr lst)))
        ;; 直接调用 f，传入所有参数
        (f h t (lambda () (nd-recur f z t))))))



;;; ======================
;;; 修正后的列表操作函数
;;; ======================

;; 非确定性插入 (修正版)
(define (nd-insert x nd-lst)
  (nd-recur
   (lambda (h t r-thunk)
     (nd-choice
      (nd-cons x (nd-list (cons h t)))  ; 前端插入
      (nd-cons h (r-thunk))))           ; 尾部递归插入
   (nd-list (list x))  ; 基值: [x] (修正为正确的非确定性列表)
   nd-lst))

;; 排列生成 (修正版)
;; (r-thunk) 代表 t perm 之后的结果，而 (f h (r-thunk)) 代表 (nd-insert h (t perm 之后的结果))
(define (nd-perm lst)
  (letrec ((foldr
            (lambda (f z nd-lst)
              (nd-recur
               (lambda (h t r-thunk)
                 (f h (r-thunk)))
               z
               nd-lst))))

    (foldr
     ;; (lambda (x acc)
     ;;   (nd-insert x acc))
     nd-insert
     (nd-list '())    ; 初始空列表
     (nd-list lst)))) ; 将输入列表转换为非确定性列表

;;; ======================
;;; 测试函数
;;; ======================

(define (run-nd computation) computation)

(define (test-perm n)
  (let ((input (let loop ((i 1) (acc '()))
                 (if (> i n) acc (loop (+ i 1) (cons i acc))))))
    (display "Permutations of ")
    (display input)
    (display ":\n")
    (display (run-nd (nd-perm input)))
    (newline)))

;; 测试 (1 2) 和 (1 2 3) 的排列
(test-perm 2)
(newline)
(test-perm 3)
