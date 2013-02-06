# #lang rackjure

Provide some useful Clojure idioms for Racket. Where Racket and
Clojure conflict, defer to Racket not Clojure.

The original motivation is to make dictionary-heavy code less-tedious.

Asumu Takikawa's
[#lang clojure](https://github.com/takikawa/racket-clojure) showed me
what's possible, and is the basis for much of this. But `#lang
rackjure` defers to Racket conventions. For example the threading
macros are `-->` and `-->>` (note the extra hyphen) because `->` is
already spoken for with Racket's contracts. As another example, `{}`
hash literals lets you use any type of key, rather than just Clojure
`:keywords`.

In other words, the spirit of `#lang clojure` is to be compatible with
Clojure, whereas the spirit of `#lang rackjure` is to adapt a few
useful ideas from Clojure for use in Racket.

## "Threading" macros `-->` and `-->>`

Although similar to the thrush combinatior, these are macros not
functions (in Clojure as well as in `#lang rackjure`).

### `-->` or "thread first"

The `-->` macro "threads" values through a series of function
applications as the _first_ argument to each one.

### `-->>` or "thread last"

The `-->>` macro "threads" values through a series of function
applications as the _last_ argument to each one.


## Applications using `dict?`s

`(dict key)`     => `(dict-ref dict key)`
`(dict key val)` => `(dict-set dict key val)`
`(key dict)`     => `(dict-ref dict key)`
`(#f dict)`      => `#f`

The last two let the `-->` threading macro provide concise notation
for accessing nested `dict`s (for example the nested `hasheq`s from
Racket's `json` module):

    (--> dict 'a 'b 'c)

expands to

    ('c ('b ('a dict)))

which in turn expands to:

    (dict-ref (dict-ref (dict-ref dict 'a) 'b) 'c)

Note that dictionary keys are _not_ required to be Clojure style
`:keywords`. They may be any type.

Keep in mind that `dict` is a Racket generic that covers a variety of
things besides hashes.  Vectors and lists are also `dict`s.  As a
result if `v` is a `vector` then `(v 2)` is `(vector-ref 2)`.

> CAVEAT: This application syntax doesn't work for `dicts` that store
> `procedure?` as keys or values. The reason is that `#lang rackjure`
> must provide its own `#%app`. The only way it can distinguish a
> normal function application from a dictionary application is to
> check for `procedure?` in the first position.

### Not-found values

One issue is how to handle the optional last argument to `dict-ref`,
which is the value to use if the key is not found. We handle this
slightly differently than `dict-ref`:

1. We use an optional _keyword_ argument, `#:else`. This leaves arity 3
available to mean `dict-set`.

2. If this arg isn't supplied and the key isn't found, `dict-ref`
raises an error. Instead we return `#f`. This is more convenient,
especially when used with threading macro `-->`. It's smart that
`dict-ref` lets you supply a specific value to mean not-found, because
what if `#f` or `'not-found` or whatever could be a valid value in the
dict?  But it's even smarter to have not-found default to something
other than error. That way, the burden is only on code that needs to
store #f as values in a dict, and such code can the `#:else` keyword.


## Hash initialization using `{ }`

Write `(hash k0 v0 k1 v1 ...)` as `{k0 v0 k1 v1 ...}`.

Especially handy with nested hashes:

```racket
{'key "value"
      {'key "value"
       'key1 "value1"}}
```

> Note that dictionary keys are _not_ required to be Clojure style
> `:keywords`.
