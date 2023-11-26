# NeoNonEmpty

Like NonEmpty, but with a few changes:

* `Show` and `Read` instance similar to that of normal lists
* 100% safe API
* A few added/removed/changed functions (see `Data.List.NeoNonEmpty`'s module docs)
* New functions aren't tied to new versions of GHC/base

```haskell
>>> :set -XOverloadedLists
>>> aNonEmpty [1, 2, 3]
[1,2,3]
>>> read "[1, 2, 3]" :: NonEmpty Int
[1,2,3]
```
