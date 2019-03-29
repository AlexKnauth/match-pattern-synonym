#lang racket/base

(provide match
         match*
         match-define
         match-lambda
         match-let match-let* match-letrec
         define/match
         define-match-expander
         define-id-match-expander)

(require (only-in racket/match define-match-expander)
         syntax/parse/define
         (prefix-in boring- racket/match)
         (for-syntax racket/base
                     syntax/parse
                     "id-match-expander.rkt"))

(define-syntax-parser match
  [(_ target:expr [pat:pat body ...+] ...)
   #'(boring-match target [pat.pat body ...] ...)])

(define-syntax-parser match*
  [(_ [target:expr ...] [[pat:pat ...] body ...+] ...)
   #'(boring-match* [target ...] [[pat.pat ...] body ...] ...)])

(define-syntax-parser match-define
  [(_ pat:pat expr:expr)
   (syntax/loc this-syntax
     (boring-match-define pat.pat expr))])

(define-syntax-parser match-lambda
  [(_ [pat:pat body ...+] ...)
   #'(boring-match-lambda [pat.pat body ...] ...)])

(define-syntax-parser match-let
  [(_ ([pat:pat val:expr] ...) body:expr ...+)
   #'(boring-match-let ([pat.pat val] ...) body ...)])

(define-syntax-parser match-let*
  [(_ ([pat:pat val:expr] ...) body:expr ...+)
   #'(boring-match-let* ([pat.pat val] ...) body ...)])

(define-syntax-parser match-letrec
  [(_ ([pat:pat val:expr] ...) body:expr ...+)
   #'(boring-match-letrec ([pat.pat val] ...) body ...)])

(define-syntax-parser define/match
  [(_ header [[pat:pat ...] body ...+] ...)
   (syntax/loc this-syntax
     (boring-define/match header [[pat.pat ...] body ...] ...))])

(define-syntax-parser define-id-match-expander
  [(_ name:id id-match-transformer:expr)
   (syntax/loc this-syntax
     (define-syntax name
       (make-id-match-expander id-match-transformer)))]
  [(_ name:id id-match-transformer:expr normal-transformer:expr)
   (syntax/loc this-syntax
     (define-syntax name
       (make-normal+id-match-expander normal-transformer
                                      id-match-transformer)))])

