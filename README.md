# NeoNonEmpty

<!-- [![CI status badge](https://img.shields.io/github/actions/workflow/status/414owen/neononempty/haskell-ci.yml)](https://github.com/414owen/neononempty/actions/workflows/haskell-ci.yml) [![Hackage version badge](https://img.shields.io/hackage/v/neononempty)](https://hackage.haskell.org/package/neononempty) [![license](https://img.shields.io/github/license/414owen/neononempty)](https://github.com/414owen/neononempty/blob/master/LICENSE) -->

Like [NonEmpty](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List-NonEmpty.html),
but with a few changes:

* `Show` and `Read` instance similar to that of normal lists
* 100% safe API
* A few added/removed/changed functions (see `Data.List.NeoNonEmpty`'s module docs)
* API isn't tied to version of GHC/base

The show instance is the original raison d'être. When scanning textual data,
the brain can interpret uniformity more quickly. Which do you think is easier
to scan?

> x :| [y, z]
> [x, y, z]

Now imagine this in various fields of a large compound structure, and you
have two of them, and you're trying to find the difference.

```haskell
>>> :set -XOverloadedLists
>>> aNonEmpty [1, 2, 3]
[1,2,3]
>>> read "[1, 2, 3]" :: NonEmpty Int
[1,2,3]
```
