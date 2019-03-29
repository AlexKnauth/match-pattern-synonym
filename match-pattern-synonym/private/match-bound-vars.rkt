#lang racket/base

(provide pat)

(require racket/base
         racket/lazy-require
         syntax/parse
         (for-template "matcher.rkt"))

(lazy-require
 [racket/match/parse (parse)]
 [racket/match/patterns (bound-vars pats->bound-vars)])

(define (pat->bound-vars pat)
  (pats->bound-vars parse (list pat)))

(define-syntax-class pat
  #:attributes [pat matcher [out 1]]
  [pattern pat:expr
    #:with [out ...] (pat->bound-vars #'pat)
    #:with matcher #'(matcher pat [out ...])])
