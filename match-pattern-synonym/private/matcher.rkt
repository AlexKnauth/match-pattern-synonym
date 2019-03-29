#lang racket/base

(provide matcher
         matcher:)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

;; A [Matcher X (Y ...)] is a function:
;;   X -> [Maybe [List Y ...]]

(define-simple-macro (matcher pat:expr [id:id ...])
  (match-lambda [pat (list id ...)] [_ #false]))

(define-match-expander matcher:
  (syntax-parser
    [(_ mer:expr [id:id ...])
     #'(app mer (list id ...))]))
