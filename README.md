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

In addition, this provides the new patterns `~refine` and `~with`.

The `(~refine pat1 pat2)` pattern binds `pat1`'s pattern variables
so that they are available in `pat2`, and the `(~with pat expr)`
pattern matches the `pat` against the value produced by the `expr`.

Example:
```racket
> (require match-pattern-synonym)
> (match (list 5 6 7) 
    [(list a (~refine b (~with d (* 2 b))) c)
     #:when (>= d 10) 
     (list a b c d)])
(list 5 6 7 12)
```
