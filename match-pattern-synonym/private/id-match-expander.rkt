#lang racket/base

(provide prop:id-match-expander
         id-match-expander?
         make-id-match-expander
         make-normal+id-match-expander
         pat)

(require racket/match
         racket/local
         syntax/parse
         syntax/apply-transformer
         (prefix-in boring- "match-bound-vars.rkt")
         "../util/stx-traverse.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/transformer))

(define-values [prop:id-match-expander
                id-match-expander?
                id-match-expander-ref]
  (make-struct-type-property 'id-match-expander))

(define make-id-match-expander
  (local [(struct id-match-expander [transformer]
            #:property prop:id-match-expander
            (λ (self) (id-match-expander-transformer self)))]
    id-match-expander))

(define make-normal+id-match-expander
  (local [(struct normal+id-match-expander [normal id-match-transformer]
            #:property prop:procedure (struct-field-index normal)
            #:property prop:id-match-expander
            (λ (self) (normal+id-match-expander-id-match-transformer self)))]
    normal+id-match-expander))

(define (id-match-expander-transformer v)
  (unless (id-match-expander? v)
    (raise-argument-error 'id-match-expander-transformer
                          "id-match-expander?"
                          v))
  (define f (id-match-expander-ref v))
  (unless (procedure? f)
    (raise-argument-error 'prop:id-match-expander "procedure?" f))
  (f v))

(define (ctx->list ctx)
  (cond [(not ctx) '()]
        [(internal-definition-context? ctx) (list ctx)]
        [else ctx]))

(define (expand-pat stx [ctx #f])
  (let loop ([stx stx])
    (syntax-parse stx
      [m:id
       #:do [(define value (syntax-local-value #'m (λ () #f) ctx))]
       #:fail-when (and (not (id-match-expander? value)) #'m)
       "expected id-match-expander"
       (loop
        (local-apply-transformer (id-match-expander-transformer value)
                                 stx
                                 'expression
                                 (ctx->list ctx)))]
      [_
       (stx-traverse/recur stx loop)])))

(define-syntax-class (pat [ctx #f])
  #:attributes [pat matcher [out 1]]
  [pattern p
    #:with :boring-pat (expand-pat #'p ctx)])

