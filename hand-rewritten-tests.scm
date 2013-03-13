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






;;; Tests from Chapter 1 of TRS.

(test-check "testc11.tex-1" 
 (run* (q)
   fail)
 `())

(test-check "testc11.tex-2"   
  (run* (q)
    (put q #t))  
  `(#t))

;(test-check "testc11.tex-3"
;  (run* (q) 
;    fail
;    (== #t q))
;  `())

(test-check "testc11.tex-3"
  (run* (q) 
    fail
    (put q #t))
  `())

(define g fail)
  
;(test-check "testc11.tex-4"   
;  (run* (q) 
;    succeed 
;    (== #t q))
;  (list #t))

(test-check "testc11.tex-4"   
  (run* (q) 
    succeed 
    (put q #t))
  (list #t))

;(test-check "testc11.tex-5"   
;  (run* (q) 
;    succeed 
;    (== #t q))
;  `(#t))

(test-check "testc11.tex-5"   
  (run* (q) 
    succeed 
    (put q #t))
  `(#t))

;(test-check "testc11.tex-6"   
;  (run* (r) 
;    succeed
;    (== 'corn r))
;  (list 'corn))

(test-check "testc11.tex-6"   
  (run* (r) 
    succeed
    (put r 'corn))
  (list 'corn))

;(test-check "testc11.tex-7"   
;  (run* (r) 
;    succeed
;    (== 'corn r))
;  `(corn))

(test-check "testc11.tex-7"   
  (run* (r) 
    succeed
    (put r 'corn))
  `(corn))

;(test-check "testc11.tex-8"   
;  (run* (r)
;    fail
;    (== 'corn r))
;  `())

(test-check "testc11.tex-8"   
  (run* (r)
    fail
    (put r 'corn))
  `())

;(test-check "testc11.tex-9"   
;  (run* (q) 
;    succeed 
;    (== #f q))
;  `(#f))

(test-check "testc11.tex-9"   
  (run* (q) 
    succeed 
    (put q #f))
  `(#f))

;(test-check "testc11.tex-10" 
;  (run* (x)
;    (let ((x #f))
;      (== #t x)))
;  '())

(test-check "testc11.tex-10" 
  (run* (x)
    (let ((x #f))
      (fresh (t)
        (put t x)
        (put t #t))))
  '())

;(test-check "testc11.tex-11" 
;  (run* (q)
;    (fresh (x)
;      (== #t x)
;      (== #t q)))
;  (list #t))

(test-check "testc11.tex-11" 
  (run* (q)
    (fresh (x)
      (put x #t)
      (put q #t)))
  (list #t))

;(test-check "testc11.tex-12" 
;  (run* (q)
;    (fresh (x)
;      (== x #t)
;      (== #t q)))
;  (list #t))

(test-check "testc11.tex-12" 
  (run* (q)
    (fresh (x)
      (put x #t)
      (put q #t)))
  (list #t))

;(test-check "testc11.tex-13" 
;  (run* (q)
;    (fresh (x)
;      (== x #t)
;      (== q #t)))
;  (list #t))

(test-check "testc11.tex-13" 
  (run* (q)
    (fresh (x)
      (put x #t)
      (put q #t)))
  (list #t))

(test-check "testc11.tex-14"   
  (run* (x)
    succeed)
  (list `_.0))

;(test-check "testc11.tex-15"   
;  (run* (x)
;    (let ((x #f))
;      (fresh (x)
;        (== #t x))))
;  `(_.0))

(test-check "testc11.tex-15"   
  (run* (x)
    (let ((x #f))
      (fresh (x)
        (put x #t))))
  `(_.0))



;;; So far these tests are boring.  Can we do appendo?

;(define appendo
;  (lambda (l s out)
;    (conde
;      ((== '() l) (== s out))
;      ((fresh (a d res)
;         (== `(,a . ,d) l)
;         (== `(,a . ,res)  out)
;         (appendo d s res))))))

;;; What if we assume (or require) that the args to appendo are LVars?
;;; That seems to help, at least some.

(define appendo
  (lambda (l s out)
    (conde
      ((put l '())
       (put s out) ; need two puts to associate s & out with each other
       (put out s))
      ((fresh (a d res)
         (put l `(,a . ,d))
         (put out `(,a . ,res))
         ; Hmm. The problem is that a, d, and res won't be associated with the car & cdr of l & out.
         ; What if 'put' could in turn call other "sub-puts" if the RHS term contained LVars?
         ; Would this solve the problem?
         (appendo d s res))))))

(test-check "appendo-1" 
  (run* (q)
    (fresh (l s out)
      (put l '())
      (put s '())
      (appendo l s out)
      (put q out)))
  '(()))

(test-check "appendo-2"
  (run* (q)
    (fresh (l s out)
      (put l '())
      (put s '(c d e))
      (appendo l s out)
      (put q out)))
  '((c d e)))

;;; bustado!!!!
(test-check "appendo-10"
  (run 5 (q)
    (fresh (l s out)
      (put l '(a b c))
      (put s '(d e))
      (appendo l s out)
      (put q out)))
  '((a b c d e)))

#!eof

(test-check "testc11.tex-16" 
  (run* (r)
    (fresh (x y)
      (== (cons x (cons y '())) r)))
  (list `(_.0 _.1)))

(test-check "testc11.tex-17" 
  (run* (s)
    (fresh (t u)
      (== (cons t (cons u '())) s)))
  (list `(_.0 _.1)))

(test-check "testc11.tex-18" 
  (run* (r)
    (fresh (x)
      (let ((y x))
        (fresh (x)
          (== (cons y (cons x (cons y '()))) r)))))
  (list `(_.0 _.1 _.0)))

(test-check "testc11.tex-19" 
  (run* (r)
    (fresh (x)
      (let ((y x))
        (fresh (x)
          (== (cons x (cons y (cons x '()))) r)))))
  (list `(_.0 _.1 _.0)))

(test-check "testc11.tex-20" 
  (run* (q) 
    (== #f q)
    (== #t q))
  `())

(test-check "testc11.tex-21"   
  (run* (q) 
    (== #f q)
    (== #f q))
  '(#f))

(test-check "testc11.tex-22" 
  (run* (q)
    (let ((x q))
      (== #t x)))
  (list #t))

(test-check "testc11.tex-23" 
  (run* (r)
    (fresh (x)
      (== x r)))
  (list `_.0))

(test-check "testc11.tex-24" 
  (run* (q)
    (fresh (x)
      (== #t x)
      (== x q)))
  (list #t))

(test-check "testc11.tex-24b" 
  (run* (q)
    (fresh (x)
      (== #t x)
      (== q x)))
  (list #t))

(test-check "testc11.tex-24c"
  (run* (q)
    (fresh (x)
      (== x #t)
      (== q x)))
  (list #t))

(test-check "testc11.tex-24d"
  (run* (q)
    (fresh (x)
      (== q x)
      (== x #t)))
  (list #t))

(test-check "testc11.tex-24e"
  (run* (q)
    (fresh (x)
      (== x q)
      (== x #t)))
  (list #t))

(test-check "testc11.tex-24f"
  (run* (q)
    (fresh (x)
      (== x q)
      (== #t x)))
  (list #t))

(test-check "testc11.tex-25" 
  (run* (q)
    (fresh (x)
      (== x q)
      (== #t x)))
  (list #t))

(test-check "testc11.tex-26" 
  (run* (q)
    (fresh (x)
      (== (eq? x q) q)))
  (list #f))

(test-check "testc11.tex-27" 
  (run* (q)
    (let ((x q))
      (fresh (q)
        (== (eq? x q) x))))
  (list #f))

(test-check "testc11.tex-28" 
  (cond
    (#f #t)
    (#t #f))
  #f)

(test-check "testc11.tex-29" 
  (cond
    (#f succeed)
    (#t fail))
  fail)
  
(test-check "testc11.tex-30" 
  (run* (x)
    (conde
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)))
  `(olive oil))

(test-check "testc11.tex-31" 
  (run1 (x)
    (conde
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)))
  `(olive))

(test-check "testc11.tex-32" 
  (run* (x)
    (conde
      ((== 'virgin x) fail)
      ((== 'olive x) succeed)
      (succeed succeed)
      ((== 'oil x) succeed)))
  `(olive _.0 oil))

(test-check "testc13.tex-conde1"
  (run* (x)
    (conde
      ((== 'olive x) succeed)
      (succeed succeed)
      ((== 'oil x) succeed)))
  `(olive _.0 oil))
  
(test-check "testc11.tex-33" 
  (run2 (x)
    (conde
      ((== 'extra x) succeed)
      ((== 'virgin x) fail)
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)))
  `(extra olive))

(test-check "testc11.tex-34" 
  (run* (r)
    (fresh (x y)
      (== 'split x)
      (== 'pea y)
      (== (cons x (cons y '())) r)))
  (list `(split pea)))

(test-check "testc11.tex-35" 
  (run* (r)
    (fresh (x y)
      (conde
        ((== 'split x) (== 'pea y))
        ((== 'navy x) (== 'bean y)))
      (== (cons x (cons y '())) r)))
  `((split pea) (navy bean)))

(test-check "testc11.tex-36" 
  (run* (r)
    (fresh (x y)
      (conde
        ((== 'split x) (== 'pea y))
        ((== 'navy x) (== 'bean y)))
      (== (cons x (cons y (cons 'soup '()))) r)))
  `((split pea soup) (navy bean soup)))

(define teacupo
  (lambda (x)
    (conde
      ((== 'tea x) succeed)
      ((== 'cup x) succeed))))

(test-check "testc11.tex-37"   
  (run* (x)
    (teacupo x))
  `(tea cup))

(test-check "testc11.tex-38"   
  (run* (r)
    (fresh (x y)
      (conde
        ((teacupo x) (== #t y) succeed)
        ((== #f x) (== #t y)))
      (== (cons x (cons y '())) r)))
  `((#f #t) (tea #t) (cup #t)))

(test-check "testc11.tex-39"   
  (run* (r)                                                                      
    (fresh (x y z)                                                              
      (conde                                                                    
        ((== y x) (fresh (x) (== z x)))                                         
        ((fresh (x) (== y x)) (== z x)))                                        
      (== (cons y (cons z '())) r)))
  `((_.0 _.1) (_.0 _.1)))

(test-check "testc11.tex-40"   
  (run* (r)                                                                      
    (fresh (x y z)                                                              
      (conde                                                                    
        ((== y x) (fresh (x) (== z x)))                                         
        ((fresh (x) (== y x)) (== z x)))
      (== #f x)
      (== (cons y (cons z '())) r)))
  `((#f _.0) (_.0 #f)))

(test-check "testc11.tex-41" 
  (run* (q)
    (let ((a (== #t q))
          (b (== #f q)))
      b))
  '(#f))

(test-check "testc11.tex-42" 
  (run* (q)
    (let ((a (== #t q))
          (b (fresh (x)
               (== x q)
               (== #f x)))
          (c (conde
               ((== #t q) succeed)
               (succeed (== #f q)))))
      b))
  '(#f))
