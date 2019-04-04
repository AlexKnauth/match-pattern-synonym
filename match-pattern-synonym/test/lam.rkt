#lang racket/base

(require racket/list
         match-pattern-synonym)
(module+ test
  (require rackunit))


(define-pattern Symbol <- (? symbol?))
(define-pattern Number <- (? number?))

(define-pattern (Listof/length e n) #:bind [n] <-
  (~or (and '() (~with n 0))
       (and (cons e (Listof/length e (app add1 n))))))

(module+ test
  (check-match (list 'a 'b 'c 'd 'e 'f 'g)
               (Listof/length Symbol len)
               (equal? len 7)))

;; -----------------------------------------------

(struct Lit [v] #:transparent)
(struct Lam [x body] #:transparent)
(struct App [f a] #:transparent)

(define-pattern (Expof/fvs lit fvs) #:bind [fvs] <-
  (~or (and Symbol (app list fvs))
       (and (Lit lit) (~with fvs '()))
       (~refine (Lam (and Symbol x) (Expof/fvs lit body-fvs))
                (~with fvs (remove* (list x) body-fvs)))
       (~refine (App (Expof/fvs lit f-fvs) (Expof/fvs lit a-fvs))
                (~with fvs (remove-duplicates (append f-fvs a-fvs))))))

(define-pattern Exp <- (Expof/fvs Number _))
(define-pattern ClosedExp <- (Expof/fvs Number '()))


(define-pattern (Env val) #:bind [] <-
  (hash-table [Symbol val] ...))

(struct Closure [env x body])

(define-pattern (Valof lit) #:bind [] <-
  (~or (Lit lit)
       (~refine (Closure (and (Env (Valof lit)) env)
                         (and Symbol x)
                         (Expof/fvs lit fvs))
                #:when (for/and ([fv (in-list fvs)])
                         (or (eq? fv x) (hash-has-key? env fv))))))

(define-pattern Val <- (Valof Number))

(module+ test
  (check-match (Lit 5) (Expof/fvs Number fvs)
               (equal? fvs '()))
  (check-match 'x (Expof/fvs Number fvs)
               (equal? fvs '(x)))
  (check-match (Lam 'x 'x) (Expof/fvs Number fvs)
               (equal? fvs '()))
  (check-match (App 'y 'x) (Expof/fvs Number fvs)
               (equal? fvs '(y x)))
  (check-match (Lam 'x (App 'y 'x)) (Expof/fvs Number fvs)
               (equal? fvs '(y))))
