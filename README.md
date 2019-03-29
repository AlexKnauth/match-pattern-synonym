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
Defines `name` as a pattern constructor equivalent to `pattern`.

```racket
(define-pattern name expr/pattern)
(define-pattern (name param-id ...) #:bind [param-id ...] expr/pattern)
```
Defines `name` as a both a normal value and a pattern constructor.
The `expr/pattern` body is interpreted as both an expression and a
pattern.
