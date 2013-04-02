;;; Defines lub and leq operators used to define a lattice.
;;; lub will be used in the definition of put.

;;; Eventually want to be able to parameterize miniKanren via different lattices.
;;; Might have different lub and leq operators, representing different constraints.
;;; Could this help with the problem of making sure constraints work together in
;;; a sane fashion?

;;; How much of the helpers (unify, walk, ext-s, etc.) should be
;;; considered part of the lattice or LVars interface, rather than
;;; part of mK?

;;; I'm sure this code can be made more efficient (in particular, all
;;; those walk*'s are annoying).  I'm just worried about correctness for now.

;;; Lindsey says that only lub or leq needs to be defined, not both.  Can we define one in terms of the other?

(define lub
  (lambda (t1 t2 s)
    (cond
      [(unify t1 t2 s) => (lambda (s^) (walk* t2 s^))]
      [else 'top])))

(define leq
  (lambda (t1 t2 s)
    (let ([t^ (lub t1 t2 s)])
      (cond
        [(eq? 'top t^) (eq? 'top t2)]
        [else (term-equal? t2 t^ s)]))))

;;;

(define term-equal?
  (lambda (t1 t2 s)
    (eq? (unify (walk* t1 s) (walk* t2 s) empty-s) empty-s)))

;;;

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify 
                    (car u) (car v) s)))
           (and s (unify 
                    (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))

(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (ext-s x v s)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v) 
         (or 
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        (else #f)))))

(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (assq u S)) =>
       (lambda (pr) (walk (rhs pr) S)))
      (else u))))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        (else v)))))
