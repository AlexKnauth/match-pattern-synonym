#lang racket/base

(provide match
         match*
         match-define
         match-lambda
         match-let match-let* match-letrec
         define/match
         define-match-expander
         define-id-match-expander
         ~or
         ~with
         ~when
         ~refine)

(require (only-in racket/match define-match-expander)
         syntax/parse/define
         (prefix-in boring- racket/match)
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     "id-match-expander.rkt"))

(define-syntax-parser match
  #:track-literals
  [(_ target:expr [pat:pat body ...+] ...)
   #'(boring-match target [pat.pat body ...] ...)])

(define-syntax-parser match*
  #:track-literals
  [(_ [target:expr ...] [[pat:pat ...] body ...+] ...)
   #'(boring-match* [target ...] [[pat.pat ...] body ...] ...)])

(define-syntax-parser match-define
  #:track-literals
  [(_ pat:pat expr:expr)
   (syntax/loc this-syntax
     (boring-match-define pat.pat expr))])

(define-syntax-parser match-lambda
  #:track-literals
  [(_ [pat:pat body ...+] ...)
   #'(boring-match-lambda [pat.pat body ...] ...)])

(define-syntax-parser match-let
  #:track-literals
  [(_ ([pat:pat val:expr] ...) body:expr ...+)
   #'(boring-match-let ([pat.pat val] ...) body ...)])

(define-syntax-parser match-let*
  #:track-literals
  [(_ ([pat:pat val:expr] ...) body:expr ...+)
   #'(boring-match-let* ([pat.pat val] ...) body ...)])

(define-syntax-parser match-letrec
  #:track-literals
  [(_ ([pat:pat val:expr] ...) body:expr ...+)
   #'(boring-match-letrec ([pat.pat val] ...) body ...)])

(define-syntax-parser define/match
  #:track-literals
  [(_ header [[pat:pat ...] body ...+] ...)
   (syntax/loc this-syntax
     (boring-define/match header [[pat.pat ...] body ...] ...))])

(define-syntax-parser define-id-match-expander
  #:track-literals
  [(_ name:id id-match-transformer:expr)
   (syntax/loc this-syntax
     (define-syntax name
       (make-id-match-expander id-match-transformer)))]
  [(_ name:id id-match-transformer:expr normal-transformer:expr)
   (syntax/loc this-syntax
     (define-syntax name
       (make-normal+id-match-expander normal-transformer
                                      id-match-transformer)))])

;; A less-restrictive "or" pattern

(define-match-expander ~or
  (syntax-parser
    [(_ pat:pat ...)
     #:do [(define init
             (if (null? (attribute pat.out))
                 '()
                 (car (attribute pat.out))))]
     #:with [out ...]
     (for/fold ([acc init])
               ([one (in-list (attribute pat.out))])
       (filter (λ (x) (member x one bound-identifier=?)) acc))
     #:with matcher
     #'(boring-match-lambda
         [(app pat.matcher (list pat.out ...)) (list out ...)]
         ...
         [_ #false])
     #'(app matcher (list out ...))]))

;; A "with" pattern like syntax-parse's #:with clause

(define ((with/p p v) x) (p v))

(define-match-expander ~with
  (syntax-parser
    [(_ pat:pat exp:expr)
     #'(app (with/p pat.matcher exp) (list pat.out ...))]))

;; A "when" pattern like the #:when clause

(define ((when/p? v) x) v)

(define-match-expander ~when
  (syntax-parser
    [(_ exp:expr)
     #'(? (when/p? exp))]))

;; A "refine" pattern, like a dependent-intersection type

(define-syntax-parser and-match-let
  [(_ body:expr) #'body]
  [(_ [pat-stuff ... e:expr] . stuff)
   #'(boring-match e
       [pat-stuff ... (and-match-let . stuff)]
       [_ #f])])

(define-match-expander ~refine
  (λ (stx)
    (define-splicing-syntax-class refine-pat
      #:attributes [[stuff 1] [out 1]]
      [pattern {~seq :pat #:when condition:expr}
        #:with [stuff ...] #'[pat #:when condition]]
      [pattern {~seq :pat}
        #:with [stuff ...] #'[pat]])
    (syntax-parse stx
      [(_) #'_]
      [(_ p:expr) #'p]
      [(_ p:refine-pat ...)
       #'(app (λ (x) (and-match-let [p.stuff ... x]
                                    ...
                                    (list p.out ... ...)))
              (list p.out ... ...))])))

