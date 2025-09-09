;; 来自 https://scheme.com/tspl4/objects.html#./objects:s223
;; 提前分配了内存，利用递归的结构，巧妙的实现了 string-append
;; 递的过程 (wind)，构建了最终的字符串长度 n，以及初始化好的 string
;; 归的过程 (unwind)，将对应的 string set 到上一步创建好的 string 上
(define string-append
  (lambda args
    (let f ([ls args] [n 0])
      (if (null? ls)
          (make-string n)
          (let* ([s1 (car ls)]
                 [m (string-length s1)]
                 [s2 (f (cdr ls) (+ n m))])
            (do ([i 0 (+ i 1)] [j n (+ j 1)])
                ((= i m) s2)
              (string-set! s2 j (string-ref s1 i))))))))
