#lang racket
;; 在以下例子中，通过 abort-current-continuation 捕获了截止到 p-tag 的 continuation,
;; 这一 capture 是通过将当前计算的一些上下文组装好，然后传给最近的 p-tag 的 handler 来实现的。
;; 可以封装任意上下文，非常灵活。
;; 比如 (lambda () (walk (cdr l))) 封装了剩余的 walk 操作。

;; Define atom? as it's used in the book: not a pair and not the empty list.
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; Define a prompt tag. This marks the boundary for our continuation.
(define p-tag (make-continuation-prompt-tag))

;; The 'walk' function traverses the S-expression 'l'.
;; 'k-exit' is the escape function to call when the traversal is fully complete.
(define (walk l k-exit)
  (cond
    [(null? l) (k-exit)] ; Traversal of this branch is done.
    [(atom? (car l))
     ;; Found an atom.
     ;; Abort the computation up to the prompt marked by 'p-tag'.
     ;; Send two values to the prompt's handler:
     ;; 1. The atom we found: (car l)
     ;; 2. A thunk that, when called, will resume the walk: (lambda () (walk (cdr l) k-exit))
     (abort-current-continuation
      p-tag
      (car l)
      (lambda () (walk (cdr l) k-exit)))]
    [else
     ;; It's a list. We must walk the 'car' first.
     ;; The escape continuation for the 'car' is a function that will then walk the 'cdr'.
     (walk (car l) (lambda () (walk (cdr l) k-exit)))]))

;; The main function to check for two identical atoms in a row.
(define (two-in-a-row*? l)
  ;; 'check-loop' is our main consumer loop. It takes the previous atom
  ;; and the continuation thunk (k-thunk) which will produce the next atom.
  (define (check-loop prev-atom k-thunk)
    (call-with-continuation-prompt
     ;; We resume the continuation by calling the thunk.
     (lambda () (k-thunk)) // k-thunk 的调用返回下一个元素。
     p-tag
     ;; The handler receives the next atom and the *next* continuation thunk.
     (lambda (next-atom next-k-thunk)
       (if (eq? prev-atom next-atom)
           #t ; We found a match!
           ;; No match, continue the loop with the new values.
           (check-loop next-atom next-k-thunk)))))

  ;; This is the initial call to start the process.
  (call-with-continuation-prompt
   ;; Start walking the list. The final exit continuation returns #f,
   ;; meaning no matches were found if the whole list is traversed.
   (lambda () (walk l (lambda () #f)))
   p-tag
   ;; The handler for the *first* item found.
   ;; It receives the first atom and the first continuation thunk.
   (lambda (first-atom first-k-thunk)
     ;; Start the checking loop with the first atom.
     (check-loop first-atom first-k-thunk))))


;; --- Examples ---

(displayln "Running examples with call-with-continuation-prompt:")

;; Example from page 177: #t
;; l is (((food) ()) (((food))))
(printf "Input: '(((food) ()) (((food)))) -> Expected: #t -> Actual: ~a\n"
        (two-in-a-row*? '(((food) ()) (((food))))))

;; Example: #f
(printf "Input: '(a b (c d) e) -> Expected: #f -> Actual: ~a\n"
        (two-in-a-row*? '(a b (c d) e)))

;; Example: #t
(printf "Input: '(a (b b) c) -> Expected: #t -> Actual: ~a\n"
        (two-in-a-row*? '(a (b b) c)))

;; Example: #f, checks across list boundaries
(printf "Input: '((a) b) -> Expected: #f -> Actual: ~a\n"
        (two-in-a-row*? '((a) b)))

;; Example: #t, atoms are consecutive even if in different sub-lists
(printf "Input: '(a ((b) b) c) -> Expected: #t -> Actual: ~a\n"
        (two-in-a-row*? '(a ((b) b) c)))

;; Example: An empty list should be #f
(printf "Input: '() -> Expected: #f -> Actual: ~a\n"
        (two-in-a-row*? '()))

;; Example: A list with no atoms should be #f
(printf "Input: '((() ())) -> Expected: #f -> Actual: ~a\n"
        (two-in-a-row*? '((() (())))))

(two-in-a-row*? '(a (b (c (d))) () () (d)))
