# #lang rackjure

Provide a few Clojure-inspired ideas in Racket. Where Racket and
Clojure conflict, prefer Racket.

Main features:

- Threading macros `~>` and `~>>`.
- Applicable dictionaries.
- Using `{` ... `}` to initialize dictionaries.

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

> Note: Unlike Clojure, `,` is not whitespace in Racket. If you want a
> variation of the Cloure convention of using `,` to help show the
> insertion point, I suppose you could use `#||#` comments. But you'll
> probably find you don't really need to.

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


## Applicable `dict`s

`#lang rackjure` redefines `#%app` to make applications work
differently when a `dict` is in the first position:

    (dict key val)        => (dict-set dict key val)

    (dict key)            => (dict-ref dict key)
    (dict key #:else def) => (dict-ref dict key default)

And also when a `dict` is in the second position:

    (key dict)            => (dict-ref dict key)
    (key #f)              => #f

The last two variants plus the `~>` threading macro provide concise
notation for accessing nested `dict`s (for example the nested
`hasheq`s from Racket's `json` module):

    (~> dict 'a 'b 'c)

expands to:

    ('c ('b ('a dict)))

which in turn expands to:

    (dict-ref (dict-ref (dict-ref dict 'a) 'b) 'c)

Note that dictionary keys are _not_ required to be Clojure style
`:keyword`s.  They may be anything.

Keep in mind that `dict` is a Racket generic that covers a variety of
things besides `hash` and association lists.  Vectors and lists are
also `dict`s.  As a result if `v` is a `vector` then `(vector-ref v
2)` can be written simply as `(v 2)`.

> CAVEAT: This application syntax doesn't work for `dict`s that store
> `procedure?` as keys or values. The reason is that `#lang rackjure`
> must provide its own `#%app`. The only way (AFIK) it can distinguish
> a normal function application from a dictionary application is to
> check for `procedure?` in the first position. As a result, in those
> cases you'll have to use `dict-ref` and `dict-set`.

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
needs to store #f as values in a dict, and such code can the `#:else`
keyword.


## Dict initialization using `{ }`

Write `((k0 . v0)(k1 . v1) ...)` as `{k0 v0 k1 v1 ...}`.

Especially handy with nested dicts:

```racket
{'key "value"
      {'key "value"
       'key1 "value1"}}
```

The `current-curly-dict` parameter says what this exapnds to. It
defaults to `alist`, but may be set to `hash`, `hasheq` or anything
with a `(f k0 v0 k1 v1 ... ...)` signature.

