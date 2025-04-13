#!r6rs

(library (scheme-programming amb)
  (export amb require set! fail on-fail)
  (import (rename (rnrs) (set! rnrs:set!)))

  (define (current-fail)
    (error 'amb "tree exhausted"))
  
  (define (fail) (current-fail))
    
  (define (set-fail! x) (rnrs:set! current-fail x))
  
  (define-syntax amb
    (syntax-rules ()
      ((amb) (fail))
      ((amb x) x)
      ((amb x y ...)
       (let ((prev-fail current-fail))
         (call-with-current-continuation
          (lambda (sk)
            (call-with-current-continuation
             (lambda (fk)
               (set-fail!
                (lambda ()
                  (set-fail! prev-fail)
                  (fk)))
               (call-with-values (lambda () x) sk)))
            (amb y ...)))))))

  (define-syntax on-fail
    (lambda (stx)
      (syntax-case stx ()
        [(_ e ...)
         #'(amb (values) (begin e ... (amb)))])))

  (define-syntax set!
    (lambda (stx)
      (syntax-case stx ()
        ((set! id exp)
         (identifier? #'id)
         #'(let ([old-value id])
             (on-fail (rnrs:set! id old-value))
             (rnrs:set! id exp))))))
         
  (define (require p)
    (if (not p) (amb))))
