;; use define_tree.ss
;; 如下代码展示了，如何将递归的树遍历结构转化成迭代版本的结构, 最终的版本由于是尾递归,等同于命令式语言的 while/for 循环。
;; rec -> cps -> defunc -> loop
;; 其中 defunc -> loop 的版本比较难推，但也不是没有规律可寻，注意观察 inorder-defunc 和 apply-k 的逻辑以及参数, 将他们合并起来，就是合并参数
;; 特别注意树的定义，叶子节点使用专门的结构表示，不要使用 '()，因为 value 里面也有 '() ,这样会造成类型混淆。

;; 版本1
(define (inorder-rec tree)
  (if (leaf? tree)
      '()
      (append (inorder-rec (left tree))
              (cons (value tree)
                    (inorder-rec (right tree))))))


;; 版本2
(define (inorder-cps tree k)
  (if (leaf? tree)
      (k '())
      (inorder-cps (left tree)
                       (lambda (left-vals)
                         (inorder-cps (right tree)
                                              (lambda (right-vals)
                                                (k (append left-vals
                                                           (cons (value tree) right-vals)))))))))

;; defunction

;; continuation constructor
(define (make-after-left-cont tree next-k)
  (list 'after-left tree next-k))

(define (make-after-right-cont tree left-vals next-k)
  (list 'after-right tree left-vals next-k))

(define (make-halt-cont)
  (list 'halt))

(define (halt? cont)
  (eq? (car cont)
       'halt))

;; 版本3
(define (inorder-rec-defunc tree cont)
  (if (leaf? tree)
      (apply-k cont '())
      (inorder-rec-defunc (left tree) (make-after-left-cont tree cont))))

;; 弹栈或者压栈
(define (apply-k cont v)
  (case (car cont)
    ((halt) v)
    ((after-left) (inorder-rec-defunc (right (cadr cont))
                                      (make-after-right-cont (cadr cont)
                                                             v
                                                             (caddr cont))))
    ((after-right) (apply-k (cadddr cont)
                            (append (caddr cont)
                                    (cons (value (cadr cont)) v))))))

;; 跳过 trampoline, 直接 loop

;; 版本4
;;(inorder-rec-loop sample-tree (make-halt-cont))
;; 从 defunc 版本推导出以下循环的思路：
;; 注意观察 inorder-defunc 和 apply-k 的逻辑以及参数。将他们合并起来，就是合并参数
;; 注意观察两者共同的参数都是 cont，比较不同的参数 tree 和 v：
;; 一个是 tree, 一个是 v。
;; 再观察逻辑，对于 tree，inorder-defunc "向下"递归并压栈 当前层级的树节点 以及 当前的 cont。
;; 观察 apply-k, 根据 cont 的类型进行 “向上”弹栈或者压栈操作。
;; 因此，我们可以将 tree / v 的逻辑合并到一起，根据 obj 的类型决定走 ”向下" 还是 "向上" 的操作。
(define (inorder-rec-loop obj cont)
  ;; 这里容易出现逻辑思维混乱，特别注意:
  ;; 由于我们一开始使用了 '() 来表示空树，于是就没法将 obj 是空树还是空值区分，于是空值也走了 tree? 的 case, 造成无限递归。
  ;; 编码过程中，虽然是动态语言，但类型必须要清晰，不清晰的时候，使用 Ocaml 写一遍。
  (if (tree? obj) ;; obj 是树，根据当前树节点类型，决定是继续深入树压栈，还是达到了左边的叶子节点，开始弹栈。
      (if (leaf? obj)
          (inorder-rec-loop '() cont)
          (inorder-rec-loop (left obj) ;; node case
                            (make-after-left-cont obj cont)))

      (if (halt? cont) ;; obj 是值 (空列表或者非空列表），开始直接弹栈
          obj
          (let ((tag (car cont))
                (tree (cadr cont)))
            (case tag
              ((halt) obj)
              ((after-left) (inorder-rec-loop (right (cadr cont))
                                              (make-after-right-cont (cadr cont)
                                                                     obj
                                                                     (caddr cont))))
              ((after-right) (inorder-rec-loop (append (caddr cont)
                                                       (cons (value (cadr cont))
                                                             obj))
                                               (cadddr cont)))

              (else (error (car cont) "unkown continuation")))))))
