mutable-match-lambda [![Build Status](https://travis-ci.org/AlexKnauth/mutable-match-lambda.png?branch=master)](https://travis-ci.org/AlexKnauth/mutable-match-lambda)
===

extendable mutable functions

documentation: http://pkg-build.racket-lang.org/doc/mutable-match-lambda/index.html

```racket
> (define my+ (mutable-match-lambda))
> my+
#<procedure:my+>
> (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? number? ns) ...) (apply + ns)])
> (my+ 1 2)
3
> (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? vector? vs) ...) (apply vector-map + vs)])
> (my+ #(1 2) #(3 4))
'#(4 6)
```
