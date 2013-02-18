# #lang rackjure

Provide some useful Clojure idioms for Racket. Where Racket and
Clojure conflict, defer to Racket not Clojure.

Asumu Takikawa's
[#lang clojure](https://github.com/takikawa/racket-clojure) showed me
what's possible, and is the basis for much of this. But `#lang
rackjure` defers to Racket conventions.

For example the threading macros are `~>` and `~>>` (using `~` instead
of `-`) because Racket already uses `->` for contracts. Plus as Danny
Yoo pointed out, `~` is more "thready".

As another example, the `{}` hash literals let you use anything for a
key, not just Clojure `:keyword`s.

In other words, the spirit of `#lang clojure` is to be compatible with
Clojure, whereas the spirit of `#lang rackjure` is to adapt a few
useful ideas from Clojure for use in Racket.

## "Threading" macros `~>` and `~>>`

Although similar to the thrush combinatior, these are macros not
functions (in Clojure as well as in `#lang rackjure`).

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

Notice that `bytes-length` and `string->bytes/utf-8` are not enclosed
in parentheses. They can be, but if they're not, the `~>` macro adds
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

`#lang rackjure` redefines `#:app` to make applications work
differently when a `dict` is in a certain position:

    (dict key val)        => (dict-set dict key val)

    (dict key)            => (dict-ref dict key)
    (dict key #:else def) => (dict-ref dict key default)

    (key dict)            => (dict-ref dict key)
    (#f dict)             => #f

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
things besides hashes.  Vectors and lists are also `dict`s.  As a
result if `v` is a `vector` then `(v 2)` is `(vector-ref 2)`.

> CAVEAT: This application syntax doesn't work for `dict`s that store
> `procedure?` as keys or values. The reason is that `#lang rackjure`
> must provide its own `#%app`. The only way (AFIK) it can distinguish
> a normal function application from a dictionary application is to
> check for `procedure?` in the first position.

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


## Hash initialization using `{ }`

Write `(hash k0 v0 k1 v1 ...)` as `{k0 v0 k1 v1 ...}`.

Especially handy with nested hashes:

```racket
{'key "value"
      {'key "value"
       'key1 "value1"}}
```

The `current-curly-dict` parameter says what this exapnds to. It
defaults to `hash`, but may be `hasheq` or anything with a similar `(f
k0 v0 k1 v1 ... ...)` signature.
