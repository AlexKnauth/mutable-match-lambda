#lang scribble/manual
@(require scribble/eval racket/sandbox)
@(require racket/base)
@(require mutable-match-lambda)
@(require (for-label mutable-match-lambda
                     racket/base
                     racket/match
                     racket/format
                     racket/local
                     racket/contract
                     racket/list
                     racket/function
                     racket/bool
                     racket/math
                     racket/vector))

@title{mutable-match-lambda}

@defmodule[mutable-match-lambda]{

These forms and functions allow a mutable generic procedure like this:

@examples[
  (require mutable-match-lambda racket/vector)
  (define my+ (mutable-match-lambda))
  my+
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? number? ns) ...) (apply + ns)])
  (my+ 1 2)
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? vector? vs) ...) (apply vector-map + vs)])
  (my+ #(1 2) #(3 4))
]}

@section{mutable-match-lambda-procedure}

@defstruct*[mutable-match-lambda-procedure ([name any/c] [procs (listof procedure?)]) #:mutable #:transparent]{
represents a procedure, with a @racket[prop:procedure] property that allows it to be applied as a procedure.
It tries each of its @racket[procs] in order, using @racket[mutable-match-lambda-clause-append].  
Forms like @racket[mutable-case-lambda] and @racket[mutable-match-lambda] all create instances of this.  
When they do, each clause is converted to a procedure (using @racket[clause->proc]) and the list of procedures
is stored in the @racket[procs] field.  
}

@defproc[(make-mutable-match-lambda [proc procedure?] ... [#:name name any/c #f]) mutable-match-lambda-procedure?]{
equivalent to @racket[(mutable-match-lambda-procedure name (list proc ...))].
}

@deftogether[(@defproc[(mutable-match-lambda-add-clause-proc! [proc mutable-match-lambda-procedure?] [clause-proc procedure?] ...) void?]
              @defproc[(mutable-match-lambda-add-overriding-clause-proc! [proc mutable-match-lambda-procedure?] [clause-proc procedure?] ...) void?])]{
these functions add clauses to a @racket[mutable-match-lambda-procedure].  
The difference between them is that @racket[mutable-match-lambda-add-clause-proc!] adds a clause that is only used when no other clause matches,
and @racket[mutable-match-lambda-add-overriding-clause-proc!] adds a clause that overrides the other clauses when it matches.

They are defined like this:
@(racketblock
  (define (mutable-match-lambda-add-clause-proc! proc . clause-procs)
    (set-mutable-match-lambda-procedure-procs! proc
                                               (append (mutable-match-lambda-procedure-procs proc)
                                                       clause-procs)))
  
  (define (mutable-match-lambda-add-overriding-clause-proc! proc . clause-procs)
    (set-mutable-match-lambda-procedure-procs! proc
                                               (append clause-procs
                                                       (mutable-match-lambda-procedure-procs proc))))
  )

@examples[
  (require mutable-match-lambda racket/vector)
  (define my+ (make-mutable-match-lambda))
  (mutable-match-lambda-add-clause-proc! my+ (clause->proc #:match-lambda* [(list (? number? ns) ...) (apply + ns)]))
  (my+ 1 2)
  (mutable-match-lambda-add-clause-proc! my+ (clause->proc #:match-lambda* [(list (? vector? vs) ...) (apply vector-map + vs)]))
  (my+ #(1 2) #(3 4))
  (mutable-match-lambda-add-clause-proc! my+ (lambda args 7))
  (my+ 1 2)
  (my+ #(1 2) #(3 4))
  (my+ "not a number or a vector")
  (mutable-match-lambda-add-overriding-clause-proc! my+ (lambda args 42))
  (my+ 1 2)
]}

@deftogether[(@defform*[((mutable-match-lambda-add-clause! proc-expr clause-proc-expr ...)
                         (mutable-match-lambda-add-clause! proc-expr kw clause ...))]
              @defform*[((mutable-match-lambda-add-overriding-clause! proc-expr clause-proc-expr ...)
                         (mutable-match-lambda-add-overriding-clause! proc-expr kw clause ...))])]{
these forms add clauses to a @racket[mutable-match-lambda-procedure].  
The first form (for both) adds the @racket[clause-proc-expr]s to the list of procs, and is
exactly like @racket[mutable-match-lambda-add-clause-proc!] and @racket[mutable-match-lambda-add-overriding-clause-proc!].  
The second form (for both) converts the @racket[clause]s to procedures (using @racket[(clause->proc kw clause)]),
and then adds those to the list of procs.  
The difference between them is the same as the difference between
@racket[mutable-match-lambda-add-clause-proc!] and
@racket[mutable-match-lambda-add-overriding-clause-proc!].  

@examples[
  (require mutable-match-lambda racket/vector)
  (define my+ (make-mutable-match-lambda))
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? number? ns) ...) (apply + ns)])
  (my+ 1 2)
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? vector? vs) ...) (apply vector-map + vs)])
  (my+ #(1 2) #(3 4))
  (mutable-match-lambda-add-clause! my+ (lambda args 7))
  (my+ 1 2)
  (my+ #(1 2) #(3 4))
  (my+ "not a number or a vector")
  (mutable-match-lambda-add-overriding-clause! my+ (lambda args 42))
  (my+ 1 2)
]}

@defproc[(mutable-match-lambda-clause-append [proc procedure?] ...
                                             [#:name name any/c 'mutable-match-lambda]) procedure?]{
makes a new procedure that tries all of the @racket[proc]s in order.

This is what @racket[mutable-match-lambda-procedure] uses to combine its clauses.  
}

@defproc[(mutable-match-lambda-next) any]{
if it is called within a mutable-match-lambda-clause procedure, it signals to
@racket[mutable-match-lambda-clause-append] to try the next clause (if there is one).
Otherwise it raises an error.

It is similar in spirit to @racket[match]'s @racket[failure-cont], except that it does escape the
current context, and it cares about the dynamic extent, not syntactic scope.
}

@defproc[(mutable-match-lambda-append [proc procedure?] ... [#:name name any/c #f])
         mutable-match-lambda-procedure?]{
makes a new @racket[mutable-match-lambda-procedure] that tries all of the @racket[proc]s in order.

The difference between this and @racket[make-mutable-match-lambda] is that if a @racket[proc] is a
@racket[mutable-match-lambda-procedure], then its procs are spliced into the resulting list.  
As a result the mutable-match-lambda-procedures are effectively copied (and in fact
@racket[mutable-match-lambda-copy] is defined with @racket[mutable-match-lambda-append]).

If you don't want this copying behavior, you can use @racket[make-mutable-match-lambda] to achieve
that.
}

@defproc[(mutable-match-lambda-copy [proc procedure?]) mutable-match-lambda-procedure?]{
if @racket[proc] is a @racket[mutable-match-lambda-procedure], it returns a copy of @racket[proc].
Otherwise it returns a @racket[mutable-match-lambda-procedure] that has @racket[proc] as its only
clause.

It is equivalent to
@racket[(mutable-match-lambda-append proc #:name (mutable-match-lambda-procedure-name proc))].
}

@defform[(make-mutable-match-lambda/infer-name proc-expr ...)]{
like @racket[make-mutable-match-lambda], except that it can infers the name argument from the context.
For example, in @racket[(define my-proc (make-mutable-match-lambda/infer-name))], it infers the name
@racket['my-proc].  
@margin-note{see @secref["infernames" #:doc '(lib "scribblings/reference/reference.scrbl")]}

It is used to infer the names for forms like @racket[mutable-case-lambda] and
@racket[mutable-match-lambda].  
}

@section{mutable-match-lambda, etc}

@defform[(mutable-case-lambda case-lambda-clause ...)]{
like @racket[case-lambda], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[case-lambda] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-case-lambda clause ...)
    (make-mutable-match-lambda/infer-name
     (clause->proc #:case-lambda clause) ...))
  )

@examples[
  (require mutable-match-lambda)
  (examples)
]}

@defform[(mutable-match-lambda match-lambda-clause ...)]{
like @racket[match-lambda], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[match-lambda] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-match-lambda clause ...)
    (make-mutable-match-lambda/infer-name
     (clause->proc #:match-lambda clause) ...))
  )

@examples[
  (require mutable-match-lambda)
  (examples)
]}

@defform[(mutable-match-lambda* match-lambda*-clause ...)]{
like @racket[match-lambda*], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[match-lambda*] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-match-lambda* clause ...)
    (make-mutable-match-lambda/infer-name
     (clause->proc #:match-lambda* clause) ...))
  )

@examples[
  (require mutable-match-lambda)
  (examples)
]}

@section{make-clause-proc and clause->proc}

@defproc[(make-clause-proc [test-proc procedure?] [then-proc procedure?]) procedure?]{
makes a procedure that @racket[mutable-match-lambda-procedure] can use as a clause-proc.  
When it is called, it calls @racket[test-proc] with it's arguments, 
and if @racket[test-proc] returns a true value, it then calls @racket[then-proc] with its arguments. 
If @racket[test-proc] returns @racket[#false], then it moves on to the next clause (if there is one). 

@examples[
  (require mutable-match-lambda racket/vector)
  (define clause-1 (make-clause-proc (λ args (andmap number? args))
                                     (λ args (apply + args))))
  (define clause-2 (make-clause-proc (λ args (andmap vector? args))
                                     (λ args (apply vector-map + args))))
  (define my+
    (make-mutable-match-lambda clause-1 clause-2))
  (my+ 1 2)
  (clause-1 1 2)
  (my+ #(1 2) #(3 4))
  (clause-2 #(1 2) #(3 4))
]}

@defform[(clause->proc kw clause)]{
makes a procedure that @racket[mutable-match-lambda-procedure] can use as a clause-proc.
The keyword specifies what type of clause it is.  

For example @racket[(clause->proc #:match-lambda* match-lambda*-clause)] creates a clause-proc that
acts like a @racket[match-lambda*] clause.  It actually expands to
@racket[(clause->proc/match-lambda* match-lambda*-clause)], and then
@racket[clause->proc/match-lambda*] does the rest.  

@racket[(clause->proc #:whatever clause)] expands to @racket[(clause->proc/whatever clause)], so if you
define a macro with the name @racket[clause->proc/whatever], then you can use
@racket[(clause->proc #:whatever clause)].  

When defining a new @racket[clause->proc/whatever] macro, it should call
@racket[mutable-match-lambda-next] if it doesn't match.  

@racket[clause->proc/case-lambda], @racket[clause->proc/match-lambda], and
@racket[clause->proc/match-lambda*] are already defined, so to start with
@racket[clause->proc] supports @racket[#:case-lambda], @racket[#:match-lambda],
and @racket[#:match-lambda*] as keywords.

@examples[
  (require mutable-match-lambda)
  (clause->proc #:case-lambda [(x y) (list x y)])
  (define-syntax-rule (clause->proc/bool->ans ans)
    (lambda (x)
      (if x
          ans
          (mutable-match-lambda-next))))
  (define f
    (make-mutable-match-lambda
     (clause->proc #:bool->ans 42)
     (lambda _ "didn't match")))
  (f #t)
  (f #f)
]}

@deftogether[[
  @defform[(clause->proc/case-lambda clause)]
  @defform[(clause->proc/match-lambda clause)]
  @defform[(clause->proc/match-lambda* clause)]
]]{
these forms produce procedures that @racket[mutable-match-lambda-procedure] can use as clause-procs,
so you can use @racket[#:case-lambda] etc. as keywords in @racket[clause->proc].
}
