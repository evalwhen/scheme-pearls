;; 二叉树结构定义，注意树有两种节点，思考其类型，使用 Ocaml
(define (leaf) (list 'leaf))  ; 空节点
(define (node value left right)
  (list 'node value left right))  ; 节点构造器

(define (leaf? tree)
  (and (not (null? tree))
       (eq? (car tree) 'leaf)))

(define (node? tree)
  (and (not (null? tree))
       (eq? (car tree) 'node)))

(define (tree? tree)
  (or (leaf? tree) (node? tree)))

(define (value tree)
  (cadr tree))

(define (left tree)
  (caddr tree))

(define (right tree)
  (cadddr tree))

;; 示例二叉树:
;;     1
;;    / \
;;   2   3
;;  / \
;; 4   5
(define sample-tree
  (node 1
        (node 2
              (node 4 (leaf) (leaf))
              (node 5 (leaf) (leaf)))
        (node 3 (leaf) (leaf))))
