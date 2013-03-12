;;; miniKanren programs, rewritten to use 'put' instead of '=='.

(load "latticeKanren.scm")

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

;(test-check "testc11.tex-1" 
;  (run* (q)
;    fail)
;  `())

(test-check "testc11.tex-1" 
  (run* (q)
    (fresh (t)
      (put t #f)
      (put t #t)))
  `())

(test-check "testc11.tex-2"   
  (run* (q)
    (put q #t))  
  `(#t))
