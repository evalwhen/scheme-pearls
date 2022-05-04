(library (tspl timer)
  (export start-timer stop-timer decrement-timer)
  (import (rnrs))

  (define clock 0)
  (define handler #f)

  (define start-timer
    (lambda (ticks new-handler)
      (set! handler new-handler)
      (set! clock ticks)))

  (define stop-timer
    (lambda ()
      (let ([time-left clock])
        (set! clock 0)
        time-left)))

  (define decrement-timer
    (lambda ()
      (when (> clock 0)
        (set! clock (- clock 1))
        (when (= clock 0) (handler)))))

  (define-syntax timed-lambda
    (syntax-rules ()
      [(_ formals exp1 exp2 ...)
       (lambda formals (decrement-timer) exp1 exp2 ...)])))

(library (tspl engines)
  (export make-engine timed-lambda)
  (import (rnrs) (tspl timer))

  (define make-engine
    (let ([do-complete #f] [do-expire #f])
      (define timer-handler
        (lambda ()
          (start-timer (call/cc do-expire) timer-handler))) ;; save current computation's (proc) continuation of engine at new-engine, and return this new engine to user.
      (define new-engine
        (lambda (resume)
          (lambda (ticks complete expire)
            ((call/cc
              (lambda (escape) ;; save the entry point of engine at do-complete and do-expire and start computation, when complete,engine returned by calling do-complete, when expire, engine returned new engine which contains remaining computation.
                 (set! do-complete
                   (lambda (ticks value)
                     (escape (lambda () (complete ticks value)))))
                 (set! do-expire
                   (lambda (resume)
                     (escape (lambda ()
                               (expire (new-engine resume))))))
                 (resume ticks)))))))
      (lambda (proc)
        (new-engine
          (lambda (ticks)
            (start-timer ticks timer-handler)
            (let ([value (proc)])
              (let ([ticks (stop-timer)])
                (do-complete ticks value))))))))

  (define-syntax timed-lambda
    (syntax-rules ()
      [(_ formals exp1 exp2 ...)
       (lambda formals (decrement-timer) exp1 exp2 ...)])))
