(coalton:define-context (non-det-dsl))

;;; ======================
;;; 类型定义 (带显式类型)
;;; ======================

;; 非确定性计算类型
(coalton:define-type (NonDet :a)
  "表示一个非确定性计算，包含多个可能结果"
  (NonDet (List (List :a))))

;;; ======================
;;; 基础操作 (带类型签名)
;;; ======================

;; 失败：无选择
(coalton:declare nd-fail (Unit -> (NonDet :a)))
(coalton:define (nd-fail)
  (NonDet Nil))

;; 确定值 -> 非确定性值
(coalton:declare nd-return (:a -> (NonDet :a)))
(coalton:define (nd-return x)
  (NonDet (Cons (Cons x Nil) Nil)))

;; 非确定性选择
(coalton:declare nd-choice ((NonDet :a) -> (NonDet :a) -> (NonDet :a)))
(coalton:define (nd-choice (NonDet xs) (NonDet ys))
  (NonDet (append xs ys)))

;; 向所有选择添加元素
(coalton:declare nd-cons (:a -> (NonDet :a) -> (NonDet :a)))
(coalton:define (nd-cons x (NonDet xss))
  (NonDet
    (map
      (lambda (xs) (Cons x xs))
      xss)))

;; 确定列表 -> 非确定性列表
(coalton:declare nd-list ((List :a) -> (NonDet :a)))
(coalton:define (nd-list xs)
  (NonDet (Cons xs Nil)))

;;; ======================
;;; 递归原语 recur (带复杂类型)
;;; ======================

;; 辅助函数：连接映射
(coalton:declare concat-map ((:a -> (List :b)) -> (List :a) -> (List :b)))
(coalton:define (concat-map f xs)
  (fold
    (lambda (x acc) (append (f x) acc))
    Nil
    xs))

;; recur 原语核心实现
(coalton:declare nd-recur
  ((:b -> (NonDet :c) -> (Unit -> (NonDet :d)) -> (NonDet :e))  ;; f 的类型
  -> (NonDet :f)                                               ;; z 的类型
  -> (NonDet :g)                                               ;; 输入列表的类型
  -> (NonDet :h)))                                             ;; 返回类型
(coalton:define (nd-recur f z (NonDet nd-lst))
  (NonDet
    (concat-map
      (lambda (choice)
        (match choice
          ((Nil)
            (match z
              ((NonDet z-val) z-val)))
          ((Cons h t)
            (match (f h (nd-list t) (lambda () (nd-recur f z (nd-list t))))
              ((NonDet result) result)))))
      nd-lst)))

;;; ======================
;;; 列表操作函数 (带精确类型)
;;; ======================

;; 非确定性插入
(coalton:declare nd-insert (:a -> (NonDet :a) -> (NonDet :a)))
(coalton:define (nd-insert x nd-lst)
  (nd-recur
    (lambda (h t r-thunk)
      (nd-choice
        (nd-cons x (nd-cons h t))
        (nd-cons h (r-thunk))))
    (nd-return x)  ; 基值: [x]
    nd-lst))

;; 右折叠实现
(coalton:declare nd-foldr
  ((:a -> (NonDet :b) -> (NonDet :c))  ;; f 的类型
  -> (NonDet :d)                      ;; z 的类型
  -> (NonDet :e)                      ;; 输入列表类型
  -> (NonDet :f)))                    ;; 返回类型
(coalton:define (nd-foldr f z nd-lst)
  (nd-recur
    (lambda (h t r-thunk)
      (f h (r-thunk)))
    z
    nd-lst))

;; 排列生成
(coalton:declare nd-perm ((List :a) -> (NonDet (List :a))))
(coalton:define (nd-perm lst)
  (nd-foldr
    nd-insert
    (nd-return Nil)  ; 初始空列表
    (nd-list lst)))

;;; ======================
;;; 运行与测试函数
;;; ======================

;; 运行非确定性计算
(coalton:declare run-nd ((NonDet :a) -> (List (List :a))))
(coalton:define (run-nd (NonDet computation))
  computation)

;; 测试函数
(coalton:declare test-perm (Integer -> Unit))
(coalton:define (test-perm n)
  (let ((input (loop :for i :from 1 :to n :collect i)))
    (progn
      (format t "Permutations of ~a:~%" input)
      (print (run-nd (nd-perm input)))
      (terpri))))

;; 执行测试
(test-perm 2)
(test-perm 3)
