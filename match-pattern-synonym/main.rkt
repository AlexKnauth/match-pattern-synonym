#lang racket/base

(provide match
         match*
         match-define
         match-lambda
         match-let match-let* match-letrec
         define/match
         define-pattern
         <-)

(require "private/match.rkt"
         "private/pattern-synonym.rkt")
