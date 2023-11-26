# NeoNonEmpty

Like [NonEmpty](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List-NonEmpty.html),
but with a few changes:

* `Show` and `Read` instance similar to that of normal lists
* 100% safe API
* A few added/removed/changed functions (see `Data.List.NeoNonEmpty`'s module docs)
* API isn't tied to version of GHC/base

```haskell
>>> :set -XOverloadedLists
>>> aNonEmpty [1, 2, 3]
[1,2,3]
>>> read "[1, 2, 3]" :: NonEmpty Int
[1,2,3]
```
