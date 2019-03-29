# match-pattern-synonym

Recursive pattern synonyms for `racket/match`.

```racket
(require match-pattern-synonym)
```
Provides `define-pattern`, a form for defining pattern synonyms.

```racket
(define-pattern name <- pattern)
(define-pattern (name param-id ...) #:bind [param-id ...] <- pattern)
```
