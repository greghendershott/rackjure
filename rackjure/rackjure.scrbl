#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          (for-label rackjure/alist
                     rackjure/conditionals
                     rackjure/dict
                     rackjure/egal
                     rackjure/str
                     rackjure/threading
                     rackjure/utils
                     racket))

@(define EVAL
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'rackjure)))

@title{#lang rackjure}

Provide a few Clojure-inspired ideas in Racket. Where Racket and
Clojure conflict, prefer Racket.

@hyperlink["https://github.com/greghendershott/rackjure" "Source"].

@margin-note{This is tested on Racket versions 6.0 and newer.}

@[table-of-contents]

@section{Background/philosophy}

Asumu Takikawa's
@hyperlink["https://github.com/takikawa/racket-clojure" "#lang
clojure"] showed me what's possible and is the basis for much of this.
Why not just use that? Because I wanted to use some Clojure ideas in
Racket, not use Clojure.

For example the threading macros are @racket[~>] and @racket[~>>]
(using @tt{~} instead of @tt{-}) because Racket already uses
@racket[->] for contracts. Plus as Danny Yoo pointed out to me, @tt{~}
is more "thready".

As another example, the @tt{{}} dictionary literals let you use anything
for a key, not just Clojure map @tt{:keyword}s.

When it must choose, @tt{#lang rackjure} chooses to be more Rackety.

@;--------------------------------------------------------------------

@section{Installation}

To install Rackjure with Racket 5.3.4 and newer:

@codeblock{
raco pkg install rackjure
}

On older versions of Racket (either you'll need Git or download the tarball
and extract them manually):

@codeblock{
git clone https://github.com/greghendershott/rackjure.git
raco link rackjure
raco setup rackjure
}

@;--------------------------------------------------------------------
@section{@tt{#lang rackjure} vs. @racket[require]}

@defmodule[rackjure]

Most features work if you merely @tt{(require rackjure)} --- or a
specific module such as @tt{(require rackjure/threading)} --- in any
module language such as @tt{racket} or @tt{racket/base}.

However a few features only work as a module language --- by using
@tt{#lang rackjure} at the start of your source file, or by supplying
@tt{rackjure} as the language in a @racket[module] form. This is
because they depend on redefining @racket[#%app] or extending the
Racket reader.  These are:

@itemize[
@item{@secref["dict-app"].}
@item{@secref["dict-init"].}
@item{@secref["func-lit"].}
]

Of course, because they must make @racket[#%app] do more work at
runtime, there is some performance overhead.

@margin-note{However the overhead is only for function applications
@emph{within} a module using @tt{rackjure} as its language ---
not for function applications in other modules.}

If you do not need those features, you can @racket[(require rackjure)]
or even just the specific modules you use, in a "leaner" @tt{lang}
such as @tt{racket/base}.

For example you can use just the threading macros @racket[~>] and
@racket[~>>] in @tt{racket/base}:

@codeblock{
#lang racket/base
(require rackjure/threading)
}

@;--------------------------------------------------------------------

@section{Threading macros}

@defmodule[rackjure/threading]

@defform[(~> expression form ...)]{

Threads @racket[expression] through the forms. Inserts
@racket[expression] as the second item in the first @racket[form],
making a list of it if it is not a list already. If there are more
forms, inserts the first form as the second item in second form, etc.

}

@defform[(~>> expression form ...)]{

Like @racket[~>] but inserting as the @italic{last} item in each form.

}

@linebreak[]

The "threading" macros let you thread values through a series of
applications in data-flow order. This can be a really refreshing way
to express a series of transforms, instead of nested function calls.

Although similar to the thrush combinator function (and you may hear
them described that way), these are actually macros (in both Clojure
and @tt{#lang rackjure}).

And although similar to @racket[compose], the order is reversed, and
again, these are macros.

The @racket[~>] form "threads" values through a series of forms as the
@italic{second} item of each form. (If a form is a function
application, remember that the second item is the first argument.)

For example, instead of:

@racketblock[
(string->bytes/utf-8 (number->string (bytes-length #"foobar") 16))
]

You can write:

@racketblock[
(~> #"foobar"
    bytes-length
    (number->string 16)
    string->bytes/utf-8)
]

Or if you prefer on one line:

@racketblock[
(~> #"foobar" bytes-length (number->string 16) string->bytes/utf-8)
]

Notice that @racket[bytes-length] and @racket[string->bytes/utf-8]
aren't enclosed in parentheses. They could be, but if they're not, the
@racket[~>] macro adds them automatically. A function that takes just one
argument can be specified this way.

@defform[(some~> expression form ...)]{

Analogous to @tt{some->} in Clojure, i.e. stop threading at a
@racket[#f] value.

}

@defform[(some~>> expression form ...)]{

Analogous to @tt{some->>} in Clojure, i.e. stop threading at a
@racket[#f] value.

}

@;--------------------------------------------------------------------

@section[#:tag "dict-app"]{Applicable dictionaries}

@tt{#lang rackjure} redefines @racket[#%app] to make applications work
differently when a @racket[dict?] is in the @italic{first}
position:

@#reader scribble/comment-reader
(racketblock
;; When (dict? d) is #t

;; Set
(d key val)            => (dict-set d key val)

;; Get
(d key)                => (dict-ref d key #f)
(d key #:else default) => (dict-ref d key default)
)

And also when a @racket[dict?] is in the @italic{second} position:

@#reader scribble/comment-reader
(racketblock
;; Get
(key d)  => (dict-ref d key)
(key #f) => #f  ; unless (or (procedure? `key`) (dict? `key`))
)

These last two variants, in combination with the @racket[~>] threading
macro, provide concise notation for accessing nested
@racket[dictionary] (for example the nested @racket[hasheq]s from Racket's
@racket[json] module):

@codeblock{
(~> dict 'a 'b 'c)
}

expands to:

@codeblock{
('c ('b ('a dict)))
}

which in turn is applied as:

@racketblock[
(dict-ref (dict-ref (dict-ref dict 'a) 'b) 'c)
]

Note that dictionary keys are not required to be Clojure style
@tt{:keyword}s. They may be anything.

@margin-note{This application syntax doesn't work for a @racket[dict?]
that stores @racket[procedure?] as keys or values. The reason is that
@tt{#lang rackjure} must provide its own @racket[#%app]. The only
way (AFAIK) it can distinguish a normal function application from a
dictionary application is to check for @racket[procedure?] in the
first position. As a result, in those cases you'll have to use
@racket[dict-ref] and @racket[dict-set].}

Keep in mind that a @racket[dict?] is a Racket generic that covers
a variety of things besides hash tables and association lists, such as
@racket[vector]s and @racket[list]s. As a result if @racket[v] is a
@racket[vector] then @racket[(vector-ref v 2)] can be written simply
as @racket[(v 2)].

@subsection{Not-found values}

One issue is how to handle the optional last argument to @racket[dict-ref],
which is the value to use if the key is not found. We handle this
slightly differently than @racket[dict-ref]:

1. We use an optional keyword argument, @racket[#:else]. This leaves
arity 3 available to mean @racket[dict-set].

2. If this arg isn't supplied and the key isn't found we return @racket[#f]
(whereas @racket[dict-ref] raises an error). Returning @racket[#f] is
more convenient, especially when used with threading macro
@racket[~>]. It's smart that @racket[dict-ref] lets you supply a
specific value to mean not-found, because what if @racket[#f] or
@racket['not-found] or whatever could be a valid value in the
dictionary? But it's even smarter to have not-found default to
something other than raising an error. That way, the burden is only on
code that needs to store @racket[#f] as values in a dict, and such
code can use the @racket[#:else] keyword.

@;----------------------------------------------------------------------------
@section[#:tag "dict-init"]{Dictionary initialization using @racket[{}]}

@tt{#lang rackjure} provides a more-concise way to create dictionaries.

You can write

@racketblock[
((k0 . v0)(k1 . v1) ...)
]

as

@racketblock[
{k0 v0 k1 v1 ... ...}
]

Especially handy with nested dicts:

@racketblock[
{'key "value"
 'key1 {'key "value"
        'key1 "value1"}}
]

The @racket[current-curly-dict] parameter says what this expands to.

@defparam[current-curly-dict v procedure?]{

Defaults to @racket[alist]. May be set to @racket[hash],
@racket[hasheq] or anything with the same @racket[(f k v ... ...)]
signature.

Examples:

@codeblock{
> (parameterize ([current-curly-dict alist])
    {'k0 0 'k1 1})
'((k0 . 0) (k1 . 1))
> (parameterize ([current-curly-dict hasheq])
    {'k0 0 'k1 1})
'#hasheq((k0 . 0) (k1 . 1))
}

}

@defmodule[rackjure/alist]

@defproc[(alist [key any/c] [val any/c] ... ...) (listof (cons any/c any/c))]{

Creates an association list.

@examples[#:eval EVAL
(alist 'k0 0 'k1 1 'k2 2)
]

}

@;----------------------------------------------------------------------------
@section{Dictionary utilities}

@defmodule[rackjure/dict]

A few utility functions for @racket[dict]s.

@defproc[(dict-merge [d0 dict?] [d1 dict?]) dict?]{

Functionally merge @racket[d1] into @racket[d0]. Values in @racket[d0]
are overriden by values with the same key in @racket[d1]. Nested
@racket[dict]s are handled recursively.

@codeblock{
> (dict-merge {} {'type 'line})
'((type . line))
> (dict-merge {'type 'triangle 'sides  3}
              {'type 'square   'sides  4})
'((type . square) (sides . 4))
> (dict-merge {'people {'john {'age 10}
                        'mary {'age 7}}}
              {'people {'john {'age 11}}})
'((people (john (age . 11)) (mary (age . 7))))
}

Setting a value in @racket[d1] to the current value of the
@racket[dict-merge-delete-value] parameter -- which defaults to
@racket['DELETE] -- causes the key/value in @racket[d0] with that key
to be deleted from the returned dictionary.

@codeblock{
> (dict-merge '([a . a][b . b])
              '([b . DELETE]))
'([a . a])
}

@defparam[dict-merge-delete-value v any/c]{
   
Defaults to @racket['DELETE]. Used to tell @racket[dict-merge] that a
key/value pair with that key should be deleted.

@codeblock{
> (parameterize ([dict-merge-delete-value 'DELETE])
    (dict-merge '([a . a]
                  [b . b])
                '([b . DELETE])))
'([a . a])
> (parameterize ([dict-merge-delete-value 'FOO])
    (dict-merge '([a . a]
                  [b . b])
                '([a . DELETE]
                  [b . FOO])))
'((a . DELETE))
}

}
}

@defproc[(dict->curly-string [d dict?]) string?]{

Returns a @tt{{}} @racket[style] string describing the @racket[dict]
@racket[d], including any nested @racket[dict]s.

@codeblock{
> (define sample-dict '([a . 0]
                        [b . 0]
                        [c . ([a . 0]
                              [b . 0]
                              [c . ([a . 0]
                                    [b . 0]
                                    [c . 0])])]))
> (displayln (dict->curly-string sample-dict))
{'a 0
 'b 0
 'c {'a 0
     'b 0
     'c {'a 0
         'b 0
         'c 0}}}
}

}

@;----------------------------------------------------------------------------
@section{Strings}

@defmodule[rackjure/str]

@defproc[(str
[expression any/c] ...
[#:fmt fmt ~a]
[#:sep sep ""]
) (and/c string? immutable?)]{

@racket[str] can be a succinct alternative to @racket[string-append]
and/or @racket[format].

Also, it returns an immutable string (created via
@racket[string->immutable-string]).


@examples[#:eval EVAL
(str)
(str "hi")
(str 1)
(str #f)
(str "Yo" "Yo")
(str "Yo" "Yo" "Ma")
(apply str '(0 1 2 3))
(str 0 1 2 3)
(str '(0 1 2 3))
]

Our version adds optional keyword arguments, the defaults of which
behave like Clojure's @tt{str}:

@itemize[

@item{@racket[#:fmt]: The function to apply to each argument. Defaults
to @racket[~a]. May be any @racket[(any/c . -> . string?)] function,
e.g. @racket[~v].}

@item{@racket[#:sep]: A @racket[string?] to add between each. Defaults
to @racket[""].}

]

@examples[#:eval EVAL
(str #:fmt ~v "Yo" "Yo")
(str #:sep " " "Yo" "Yo")
(str #:fmt ~v  #:sep " " "Yo" "Yo")
]

}

@;----------------------------------------------------------------------------
@section{Conditionals}

@defmodule[rackjure/conditionals]

@defform[(if-let [identifier test-expr] then-expr else-expr)]{

Combines @racket[if] and @racket[let]:

@racketblock[
(let ([identifier test-expr])
  (if identifier
      then-expr
      else-expr))
]

}

@defform[(when-let [identifier test-expr] body ...+)]{

Combines @racket[when] with @racket[let]:

@racketblock[
(let ([identifier test-expr])
  (when identifier
    body ...))
]

}

@defform[(if-not test-expr then-expr else-expr)]{

A shortcut for:

@racketblock[
(if (not test-expr)
    then-expr
    else-expr)
]

}

@defform[(when-not test-expr body ...+)]{

A shortcut for:

@racketblock[
(when (not test-expr)
  body ...)
]

However in colloquial Racket we'd simply use @racket[unless]:

@racketblock[
(unless test-expr
  body ...)
]

}

@;----------------------------------------------------------------------------
@section{Operational equivalence}

@defmodule[rackjure/egal]

@defproc[(egal? [v1 any/c] [v2 any/c]) boolean?]{

An implementation of @tt{egal?} as described in @hyperlink["http://home.pipeline.com/~hbaker1/ObjectIdentity.html" "Equal Rights for Functional Objects"].

An alternative to @racket[equal?] and @racket[eq?] that says whether
two things are "operationally equivalent", by taking into account
mutability.

In general, two things that are @racket[equal?] will also be
@racket[egal?] only if they are both immutable. Some things in Racket
aren't immutable by default. For example, although
@racket["string-constants"] are immutable, strings returned by
@racket[string] or @racket[string-join] are mutable, unless you also
run them through @racket[string->immutable-string]. Same with
@racket[bytes]. Other things come in both mutable and immutable
variants, such as hashes and vectors.

For more details, see
@hyperlink["https://github.com/greghendershott/rackjure/blob/master/rackjure/egal.rkt" "egal.rkt"]
for the implementation and test cases. A few examples:

@#reader scribble/comment-reader
(examples #:eval EVAL
(require rackjure/egal)
;; Although "string" literals are immutable...
(egal? "a" "a")
;; @racket[string] is mutable...
(egal? (string #\a) (string #\a))
;; Immutable strings are (you guessed it) immutable...
(egal? (string->immutable-string (string #\a))
       (string->immutable-string (string #\a)))
)

@subsection{@racket[egal?] and @racket[struct]s}

For two @racket[struct]s to be @racket[egal?], all of the following
must be true:

1. They must have the same field values.

2. They must be instances of the same structure type.

3. The structure type must be @racket[#:transparent]. (Regular
@racket[equal?] does a field comparison for Racket @racket[struct]s
only if they are @racket[#:transparent]. Otherwise the
@racket[struct]s are opaque and @racket[eq?] is used.)

4. The structure type must @italic{not} be @racket[#:mutable], nor
must any of the individual fields be @racket[#:mutable].

}

@;----------------------------------------------------------------------------
@section{Other}

@defmodule[rackjure/utils]

@subsection{Partial application}

@defproc[(partial [proc procedure?] [v any/c] ...) procedure?]{

Function for partial application. Differs from @racket[curry] in that
it doesn't care about function arity.

@codeblock{
((partial + 1) 2) <=> (+ 1 2)
}

}

@subsection{Atomic swap}

@defproc[(box-swap! [box box?] [proc procedure?] [v any/c] ...) any/c]{

Like @tt{swap!} in Clojure, but for @racket[box?].

Essentially it is:

@racketblock[
(define (box-swap! box f . args)
  (let loop ()
    (let* ([old (unbox box)]
           [new (apply f old args)])
      (if (box-cas! box old new)
          new
          (loop)))))
]

@margin-note{This is implemented using @racket[box-cas!] introduced in
Racket 5.92. On older versions of Racket, calling @racket[box-swap!]
raises an error.}

}

@;----------------------------------------------------------------------------
@section[#:tag "func-lit"]{Reader function literals}

The Clojure reader lets you succinctly define anonymous function
literals. For example

@codeblock{
    #(+ % %2)
}

is equivalent to this in Clojure:

@codeblock{
    (fn [% %2] (+ % %2))
}

or in Racket:

@racketblock[
    (λ (% %2) (+ % %2))
    (lambda (% %2) (+ % %2))
]

@itemize[
@item{@tt{%1} through @tt{%@italic{n}} are positional arguments}
@item{@tt{%} is a synonym for @tt{%1}}
@item{@tt{%&} is a rest argument}
@item{@tt{%#:keyword} is a @racket[#:keyword] argument}
]

The Racket reader already uses @litchar{#( )} for vector literals.
Therefore Rackjure instead uses your choice of @litchar{#fn( )},
@litchar{#λ( )}, or @litchar{#lambda( )}.

Examples:

@verbatim{
> (map #λ(+ % 1) '(1 2 3))
'(2 3 4)
> (map #λ(+ % %2) '(1 2 3) '(1 2 3))
'(2 4 6)

;; Rest argument
> (#λ(apply list* % %&) 1 '(2 3))
'(1 2 3)

;; Keyword argument
> (#λ(* 1/2 %#:m (* %#:v %#:v)) #:m 2 #:v 1)
1

;; Ignores unused arguments
> (#λ(begin %2) "ignored" "used")
"used"

;; Handles an arbitary number of arguments
> (apply #λ(list %1 %42) (build-list 42 add1))
(list 1 42)
}
