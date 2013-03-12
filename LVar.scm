;;; LVar-related definitions

(load "lattice.scm")

;;; not sure if ext-s-check is really needed in put

(define put
  (lambda (l t)
    (lambdag@ (s)
      (let ([t^ (lub l t s)])
        (cond
          [(eq? 'top t^) (mzero)]
          [else
           (unit (ext-s-check l t^ s))])))))

;;; I'm not worrying about protecting against illegal uses of consume
;;; right now.  My uses of consume will be immediately before reification.
;;;
;;; Basically, only the query variable 'q' needs to be consumed, and that happens at the end of the run.
;;;
;;; Not sure how to make consume behave usefully as a goal constructor.  Maybe something like project?
;;; For now, leave consume as a non goal-constructor.

(define consume
  (lambda (l)
    (lambda (s)
      (walk* l s))))

(define-syntax new
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s)
       (inc
         (let ((x (var 'x)) ...)
           (bind* (g0 s) g ...)))))))
