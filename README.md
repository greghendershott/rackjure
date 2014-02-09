# #lang rackjure

[![Build Status](https://travis-ci.org/greghendershott/rackjure.png?branch=master)](https://travis-ci.org/greghendershott/rackjure)

Provide a few Clojure-inspired ideas in Racket. Where Racket and
Clojure conflict, prefer Racket.

Main features:

- Threading macros `~>` and `~>>`.
- Threading macros `some~>` and `some~>>`.
- Applicable dictionaries.
- Using `{` ... `}` to initialize dictionaries.
- `str`
- `if-let` and `when-let`
- `if-not` and `when-not`
- `partial`
- `egal?`
- `box-swap!`

> **NOTE**: This is tested on recent versions of Racket. If you find an issue using a Racket version older than 5.3.2, and for some reason you can't upgrade, please [report here](https://github.com/greghendershott/rackjure/issues) and I'll try to fix if possible.

## Background/philosophy

Asumu Takikawa's
[#lang clojure](https://github.com/takikawa/racket-clojure) showed me
what's possible and is the basis for much of this. Why not just use
that? Because I wanted to use some Clojure ideas in Racket, not use
Clojure.

For example the threading macros are `~>` and `~>>` (using `~` instead
of `-`) because Racket already uses `->` for contracts. Plus as Danny
Yoo pointed out to me, `~` is more "thready".

As another example, the `{}` dictionary literals let you use anything
for a key, not just Clojure map `:keyword`s.

When it must choose, `#lang rackjure` chooses to be more Rackety.

## Installation

To install Rackjure with Racket 5.3.4 and newer:

```sh
raco pkg install rackjure
```

On older versions of Racket (either you'll need Git or download the tarball
and extract them manually):

```sh
git clone https://github.com/greghendershott/rackjure.git
raco link rackjure
raco setup rackjure
```

## "Threading" macros `~>` and `~>>`

The "threading" macros let you thread values through a series of
applications in data-flow order. This can be a really refreshing way
to express a series of transforms, instead of nested function calls.

Although similar to the thrush combinator function (and you may hear
them described that way), these are actually macros (in both Clojure
and `#lang rackjure`).

And although similar to `compose`, the order is reversed, and again,
these are macros.

### `~>` a.k.a. "thread first"

The `~>` macro "threads" values through a series of function
applications as the _first_ argument to each one.

For example, instead of:

```racket
(string->bytes/utf-8 (number->string (bytes-length #"foobar") 16))
```

You can write:

```racket
(~> #"foobar"
    bytes-length
    (number->string 16)
    string->bytes/utf-8)
```

Or if you prefer on one line:

```racket
(~> #"foobar" bytes-length (number->string 16) string->bytes/utf-8)
```

The result of `bytes-length` will be "plugged in" as the first
argument to `(number->string 16)`: `(number->string #|here|# 16)`.

> **NOTE:** Unlike Clojure, `,` is not whitespace in Racket. If you
> want a variation of the Clojure convention of using `,` to help show
> the insertion point, I suppose you could use `#||#` comments. But
> you'll probably find you don't really need to.

Notice that `bytes-length` and `string->bytes/utf-8` aren't enclosed
in parentheses. They could be, but if they're not, the `~>` macro adds
them automatically. A function that takes just one argument can be
specified this way. As a result, `~>` can also be used as a kind of
"`compose` where the arguments are in the 'common-sense' or
'data-flow' order", as opposed to the formal math order.

```racket
((compose c b a) x)  <=>  (~> x a b c)
```

### `~>>` a.k.a. "thread last"

The `~>>` macro "threads" values through a series of function
applications as the _last_ argument to each one.

## "Threading" macros `some~>` and `some~>>`

Analogous to `some->` and `some->>` in Clojure, i.e. stop threading at a `#f` value.

## Applicable `dict`s

`#lang rackjure` redefines `#%app` to make applications work
differently when a `dict` is in the first position:

```racket
;; When (dict? d) is #t

;; Set
(d key val)            => (dict-set d key val)

;; Get
(d key)                => (dict-ref d key #f)
(d key #:else default) => (dict-ref d key default)
```

And also when a `dict` is in the second position:

```racket
;; Get
(key d)  => (dict-ref d key)
(key #f) => #f  ; unless (or (procedure? `key`) (dict? `key`))
```

These last two variants, in combination with the `~>` threading macro,
provide concise notation for accessing nested `dict`s (for example the
nested `hasheq`s from Racket's `json` module):

```racket
(~> dict 'a 'b 'c)
```

expands to:

```racket
('c ('b ('a dict)))
```

which in turn is applied as:

```racket
(dict-ref (dict-ref (dict-ref dict 'a) 'b) 'c)
```

Note that dictionary keys are _not_ required to be Clojure style
`:keyword`s.  They may be anything.

Keep in mind that `dict` is a Racket generic that covers a variety of
things besides `hash` and association lists.  Vectors and lists are
also `dict`s.  As a result if `v` is a `vector` then `(vector-ref v
2)` can be written simply as `(v 2)`.

> **CAVEAT:** This application syntax doesn't work for `dict`s that
> store `procedure?` as keys or values. The reason is that `#lang
> rackjure` must provide its own `#%app`. The only way (AFAIK) it can
> distinguish a normal function application from a dictionary
> application is to check for `procedure?` in the first position. As a
> result, in those cases you'll have to use `dict-ref` and `dict-set`.

### Not-found values

One issue is how to handle the optional last argument to `dict-ref`,
which is the value to use if the key is not found. We handle this
slightly differently than `dict-ref`:

1. We use an optional _keyword_ argument, `#:else`. This leaves arity 3
available to mean `dict-set`.

2. If this arg isn't supplied and the key isn't found we return `#f`
(whereas `dict-ref` raises an error). Returning `#f` is more
convenient, especially when used with threading macro `~>`. It's smart
that `dict-ref` lets you supply a specific value to mean not-found,
because what if `#f` or `'not-found` or whatever could be a valid
value in the dict?  But it's even smarter to have not-found default to
something other than error. That way, the burden is only on code that
needs to store #f as values in a dict, and such code can use the
`#:else` keyword.


## Dict initialization using `{ }`

Write `((k0 . v0)(k1 . v1) ...)` as `{k0 v0 k1 v1 ...}`.

Especially handy with nested dicts:

```racket
{'key "value"
 'key1 {'key "value"
        'key1 "value1"}}
```

The `current-curly-dict` parameter says what this expands to. It
defaults to `alist`, but may be set to `hash`, `hasheq` or anything
with a `(f k0 v0 k1 v1 ... ...)` signature.

## `str`

`str` can be a succinct alternative to `string-append` or `format`.

Also, it returns an immutable string (created via
`string->immutable-string`).

Examples:

```racket
(str)      => ""
(str "hi") => "hi"
(str 1)    => "1"
(str #f)   => "#f"
(str "Yo" "Yo")      => "YoYo"
(str "Yo" "Yo" "Ma") => "YoYoMa"
(str '(0 1 2 3 4 5 6 7 8 9))        => "(0 1 2 3 4 5 6 7 8 9)"
(apply str '(0 1 2 3 4 5 6 7 8 9))  => "0123456789"
```

Our version adds optional keyword arguments, the defaults of which
behave like Clojure's `str`.

- `#:fmt`: The function to apply to each argument. Defaults to
           `~a`. May be any `(any/c -> string?)` function, e.g. `~v`.

- `#:sep`: A `string?` to add between each. Defaults to `""`.

Examples:

```racket
(str #:fmt ~v "Yo" "Yo")            => "\"Yo\"\"Yo\""
(str #:sep " " "Yo" "Yo")           => "Yo Yo"
(str #:fmt ~v  #:sep " " "Yo" "Yo") => "\"Yo\" \"Yo\""
```

## `if-let` and `when-let`

Handy conditionals that (as you might have guessed) combine `if`/`when` with `let`.

Examples:

```racket
(define dict {'foo 5})

(if-let [foo ('foo dict)]
  (add1 foo)
  'foo-not-found) => 6

(when-let [foo {'foo dict}]
  (displayln "OK... *drumroll*")
  (displayln (str "foo was set to " foo)))
```

## `if-not` and `when-not`

Handy shortcuts for `(if (not test) then else)` and `(when (not test) body ...+)`.

## `partial`

Function for partial application. `((partial + 1) 2)` <=> `(+ 1 2)`.
Differs from `curry` in that it doesn't care about function arity.

## `egal?`

An implementation of `egal?` as described in
[_Equal Rights for Functional Objects_]. An alternative to `equal?`
and `eq?` that says whether two things are "operationally equivalent",
by taking into account mutability.

[_Equal Rights for Functional Objects_]: http://home.pipeline.com/~hbaker1/ObjectIdentity.html

In general, two things that are `equal?` will also be `egal?` only if
they are both immutable. Some things in Racket aren't immutable by
default. For example, although `"string constants"` are immutable,
strings returned by `string` or `string-join` are not, unless you also
run them through `string->immutable-string`. Same with `bytes`. Many
things come in both mutable and immutable variants, such as hashes and
vectors.

For more details, see [`egal.rkt`] for the implementation and test
cases. A few examples:

```racket
;; Although "string" literals are immutable, `string` isn't
(egal? "a" "b") ; #f
(egal? "a" "a") ; #t
(egal? (string #\a) (string #\a)) ; #f (because neither is immutable)
(egal? (string->immutable-string (string #\a))
       (string->immutable-string (string #\a))) ; #t (b/c both are immutable)
```

[`egal.rkt`]: https://github.com/greghendershott/rackjure/blob/master/rackjure/egal.rkt

### `egal?` and `struct`s

For two `struct`s to be `egal?`, all of the following must be true:

1. They must have the same field values.

2. They must be instances of the same structure type.

3. The structure type must be `#:transparent`.

    Regular `equal?` does a field comparison for Racket `struct`s only
    if they are `#:transparent`; otherwise the `struct`s are opaque
    and `eq?` is used.

4. The structure type must not be `#:mutable`, nor must any of the
   individual fields be `#:mutable`.

## `box-swap!`

Like `swap!` in Clojure, but for [boxes]. Requires Racket 5.92+.

[boxes]: http://docs.racket-lang.org/reference/boxes.html
