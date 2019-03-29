#lang racket/base

(provide define-pattern <-)

(require syntax/parse/define
         "match.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/transformer
                     "id-match-expander.rkt"))

(define-syntax <-
  (λ (stx)
    (raise-syntax-error #f "used out of context" stx)))

(begin-for-syntax
  ;; Id -> [Syntax -> Syntax]
  (define (make-var-like-transformer id)
    (set!-transformer-procedure (make-variable-like-transformer id)))

  ;; Syntax Id -> [Syntax -> Syntax]
  (define (make-pattern-parameter-transformer param-matcher
                                              param-var)
    (define (pattern-parameter-transformer stx)
      (syntax-parse stx
        [:id
         #:with f param-matcher
         #:with x (syntax-local-identifier-as-binding
                   (syntax-local-introduce param-var))
         #'(app f (and (not #f) x))]))
    pattern-parameter-transformer)

  ;; Syntax -> [Syntax -> Syntax]
  (define (make-id-pattern-synonym-transformer matcher-id)
    (define (id-pattern-synonym-transformer stx)
      (syntax-parse stx
        [:id
         #:with f matcher-id
         #'(app f (list))]))
    id-pattern-synonym-transformer)

  ;; Syntax Natural [Listof Natural] -> [Syntax -> Syntax]
  (define (make-pattern-synonym-transformer matcher-id n is)
    (define (pattern-synonym-transformer stx)
      (syntax-parse stx
        [(_ p:pat ...)
         #:with f #`(#,matcher-id p.matcher ...)
         #:with [[x ...] ...]
         (for/list ([i (in-list is)])
           (list-ref (attribute p.out) i))
         #'(app f (list (list x ...) ...))]))
    pattern-synonym-transformer))

(define-syntax-parser define-pattern
  #:literals [<-]

  [(_ name:id
      {~optional {~and pat-only? <-}}
      {~and exp:expr pat:pat})
   #:fail-when (and (pair? (attribute pat.out)) (car (attribute pat.out)))
   "unexpected free variable in pattern"
   #:attr name/p (generate-temporary #'name)
   #:attr name/v (and (not (attribute pat-only?)) (generate-temporary #'name))
   #'(begin
       (define-id-match-expander name
         (make-id-pattern-synonym-transformer #'name/p)
         (~? (make-var-like-transformer #'name/v)))
       (~? (define name/v exp))
       (define name/p pat.matcher))]

  [(_ (name:id param:id ...)
      #:bind [bind:id ...]
      {~optional {~and pat-only? <-}}
      body:expr)
   #:attr name/p (generate-temporary #'name)
   #:attr name/v (and (not (attribute pat-only?)) (generate-temporary #'name))
   #:with param-n (length (attribute param))
   ;; (bind ...) must be a subset of (param ...)
   #:fail-when (for/or ([bind (in-list (attribute bind))])
                 (and (not (member bind (attribute param) bound-identifier=?))
                      bind))
   "not a parameter"
   #:with [bind-param-idx ...]
   (for/list ([bind (in-list (attribute bind))])
     (index-of (attribute param) bind bound-identifier=?))
   #:with [param/p ...] (generate-temporaries #'[param ...])
   #:with [param/wp ...] (generate-temporaries #'[param ...])
   #:with [param-x ...] (generate-temporaries #'[param ...])
   #:with [bind-x ...]
   (for/list ([i (in-list (syntax->datum #'(bind-param-idx ...)))])
     (list-ref (attribute param-x) i))
   #'(begin
       (define-match-expander name
         (make-pattern-synonym-transformer (quote-syntax name/p)
                                           'param-n
                                           '(bind-param-idx ...))
         (~? (make-var-like-transformer #'name/v)))
       (~? (define (name/v param ...) body))
       (define (name/p param/p ...)
         (let-syntax
             ([param
               (make-id-match-expander
                (make-pattern-parameter-transformer
                 #'param/p
                 #'param-x))]
              ...)
           (match-lambda
             [body (list bind-x ...)]
             [_    #false]))))])

;; ------------------------------------------

