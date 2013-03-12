;;; miniKanren compatibility

(define-syntax fresh
  (syntax-rules ()
    [(_ (x* ...) g g* ...)
     (new (x* ...) g g* ...)]))

;;; WAT DO???
#;(define ==
 (lambda (t1 t2)
   (lambdag@ (s)
     ((new ()
        (put t1 t2)
        (put t2 t1))
      s))))
