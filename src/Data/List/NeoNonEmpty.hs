{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PatternSynonyms     #-}

{-|
NonEmpty - Like base's NonEmpty but with:

  * 'Show' and 'Read' instance similar to `[]`
  * A completely safe API
  * added\/removed\/updated functions


Added functions:

  * 'aNonEmpty'
  * `scanl'`
  * 'sortOn'
  * 'fromCons'

  * 'groupAll'
  * 'groupAllBy'
  * 'groupAll1'
  * 'groupAllBy1'

  * 'snoc'
  * 'unsnoc'

  * 'foldl1'   -- this family is present in base's 'Foldable1' from >= 4.18.0.0
  * `foldl1'`
  * 'foldr1'

Removed functions:

  * 'NE.unzip'  (not nonempty-specific)
  * 'NE.unfold' (deprecated, use unfoldr)
  * 'NE.xor'    (seemed out of place)


Changed functions:

  * 'uncons'


Replaced functions:

  * s/(!!)/(!?)/
-}

module Data.List.NeoNonEmpty
  (
  -- * Construction
    NonEmpty
  , pattern (:|)
  , singleton
  , fromCons

  -- * Type annotations
  , aNonEmpty

  -- * Converting to and from base's 'NE.NonEmpty'
  , fromNonEmpty
  , toNonEmpty

  -- * Converting to and from lists
  , fromList
  , toList

  -- * Basic functions
  , length
  , head
  , tail
  , last
  , init
  , cons
  , uncons
  , snoc
  , unsnoc
  , unfoldr
  , sort
  , reverse
  , inits
  , inits1
  , tails
  , tails1
  , append
  , appendList
  , prependList

  -- * Stream transformations
  , map
  , intersperse
  , foldl1
  , foldl1'
  , foldr1
  , scanl
  , scanl'
  , scanl1
  , scanr
  , scanr1
  , transpose
  , sortBy
  , sortOn
  , sortWith

  -- * Building streams
  , iterate
  , repeat
  , cycle
  , insert
  , some1

  -- * Extracting sublists
  , take
  , drop
  , splitAt
  , takeWhile
  , dropWhile
  , span
  , break
  , filter
  , partition
  , group
  , groupAll
  , groupBy
  , groupAllBy
  , groupWith
  , groupAllWith
  , group1
  , groupAll1
  , groupBy1
  , groupAllBy1
  , groupWith1
  , groupAllWith1

  -- * Sublist predicates
  , isPrefixOf

  -- * "Set" operations
  , nub
  , nubBy

  -- * Indexing streams
  , (!?)

  -- * Zipping streams
  , zip
  , zipWith
  ) where

import Prelude
  ( Bool
  , Applicative(..)
  , Eq(..)
  , Foldable
  , Functor(..)
  , Int
  , Maybe(..)
  , Monad(..)
  , Ord(..)
  , Ordering(..)
  , Semigroup
  , Show(..)
  , ($)
  , (.)
  , (++)
  , fail
  , otherwise
  )

import Control.Applicative             (Alternative)
import Control.Monad.Fix               (MonadFix)
import Control.Monad.Zip               (MonadZip)
import Data.Bifunctor                  (first)
import Data.Data                       (Data)
#if MIN_VERSION_base(4,18,0)
import Data.Foldable1                  (Foldable1)
#endif
import Data.Functor.Classes            (Eq1, Ord1, Read1, Show1)
import Data.Maybe                      (fromJust, listToMaybe)
import Data.Tuple                      (swap)
import GHC.Generics                    (Generic, Generic1)
import Text.Read                       (Read(..))

import qualified Data.Foldable             as Foldable
import qualified Data.List                 as List
#if (MIN_VERSION_base_compat(0,10,1))
import qualified Data.List.NonEmpty.Compat as NE
#else
import qualified Data.List.NonEmpty        as NE
#endif
import qualified GHC.Exts                  as Exts


-- * Instances

instance Exts.IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList = NonEmpty . Exts.fromList
  toList (NonEmpty l) = NE.toList l

instance Show a => Show (NonEmpty a) where
  show (NonEmpty l) = show (NE.toList l)

instance Read a => Read (NonEmpty a) where
  readPrec = readPrec >>= \case
    (x : xs) -> pure $ NonEmpty $ x NE.:| xs
    [] -> fail "Can't read NonEmpty list with zero elements"


-- * Construction

-- | A list with one or more elements.
newtype NonEmpty a = NonEmpty (NE.NonEmpty a)
  deriving (Generic, Generic1, Data)
  deriving
    ( Applicative
    , Functor
    , MonadFix
    , MonadZip
    , Foldable
#if MIN_VERSION_base(4,18,0)
    , Foldable1
#endif
    , Eq1
    , Ord1
    , Read1
    , Show1
    , Monad
    ) via NE.NonEmpty
  deriving
    ( Eq
    , Ord
    , Semigroup
    ) via (NE.NonEmpty a)

{-# COMPLETE (:|) #-}
pattern (:|) :: a -> [a] -> NonEmpty a
pattern x :| xs <- NonEmpty (x NE.:| xs) where
  x :| xs = NonEmpty (x NE.:| xs)

-- | Construct a NonEmpty list from a single element.
singleton :: a -> NonEmpty a
singleton a = NonEmpty (a NE.:| [])

-- | Construct a 'NonEmpty' from an element and a list.
fromCons :: a -> [a] -> NonEmpty a
fromCons x xs = NonEmpty (x NE.:| xs)


-- * Unsafe utilities, not exported

onUnderlying
  :: forall a b c d. (Exts.Coercible (NonEmpty c) a, Exts.Coercible b (NonEmpty d))
  => (a -> b)
  -> NonEmpty c -> NonEmpty d
onUnderlying f = Exts.coerce . f . Exts.coerce

onNonEmpty :: (NE.NonEmpty a -> NE.NonEmpty b) -> NonEmpty a -> NonEmpty b
onNonEmpty = onUnderlying

-- Unsafe. Not exported.
unsafeFromList :: [a] -> NonEmpty a
unsafeFromList = fromJust . fromList

-- Unsafe. Not exported.
onList :: ([a] -> [b]) -> NonEmpty a -> NonEmpty b
onList f = unsafeFromList . f . toList


-- * Type annotations

-- | A non empty thing. Useful as a syntactically lightweight type annotation,
-- especially when using OverloadedLists:
--
-- >>> :set -XOverloadedLists
-- >>> [(), ()]
--     • Ambiguous type variable ‘a0’ arising from a use of ‘print’
--       prevents the constraint ‘(Show a0)’ from being solved.
-- >>> aNonEmpty [(), ()]
-- [(),()]
aNonEmpty :: NonEmpty a -> NonEmpty a
aNonEmpty a = a


-- * Converting to and from nonempties

-- | Converts base's 'NE.NonEmpty' to a 'NonEmpty'
fromNonEmpty :: NE.NonEmpty a -> NonEmpty a
fromNonEmpty = NonEmpty

-- | Converts a 'NonEmpty' to base's 'NonEmpty'
toNonEmpty :: NonEmpty a -> NE.NonEmpty a
toNonEmpty (NonEmpty ne) = ne


-- * Converting to and from lists

-- | Converts a normal list to a NonEmpty list, given the list has at least one element
fromList :: [a] -> Maybe (NonEmpty a)
fromList (x : xs) = Just (fromCons x xs)
fromList [] = Nothing

-- | Converts a 'NonEmpty' list to a normal list
toList :: NonEmpty a -> [a]
toList (NonEmpty ne) = NE.toList ne


-- * Basic functions

-- | Number of elements in NonEmpty list.
length :: NonEmpty a -> Int
length = NE.length . toNonEmpty

-- | Extract the first element of the nonempty stream.
head :: NonEmpty a -> a
head = NE.head . toNonEmpty

-- | Extract the possibly-empty tail of the nonempty stream.
tail :: NonEmpty a -> [a]
tail = NE.tail . toNonEmpty

-- | Extract the last element of the nonempty stream.
last :: NonEmpty a -> a
last = NE.last . toNonEmpty

-- | Extract everything except the last element of the nonempty stream.
init :: NonEmpty a -> [a]
init = NE.init . toNonEmpty

-- | Prepend an element to the nonempty stream.
cons :: a -> NonEmpty a -> NonEmpty a
cons el = onUnderlying (NE.cons el)

-- | Produces the first element of the nonempty stream,
-- and a stream of the remaining elements.
uncons :: NonEmpty a -> (a, [a])
uncons (NonEmpty (x NE.:| xs)) = (x, xs)

-- | Append an element to the back of a nonempty stream.
snoc :: NonEmpty a -> a -> NonEmpty a
snoc l el = l `append` singleton el

-- | Produces all elements up to the last element, and the last element
unsnoc :: forall a. NonEmpty a -> ([a], a)
unsnoc = first List.reverse . swap . uncons . reverse

-- | Dual of 'foldr', see 'List.unfoldr'.
unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr f initial = fromNonEmpty $ NE.unfoldr f initial

-- | Sort a nonempty stream.
sort :: forall a. Ord a => NonEmpty a -> NonEmpty a
sort = onNonEmpty NE.sort

-- | Reverse a nonempty stream.
reverse :: NonEmpty a -> NonEmpty a
reverse = onNonEmpty NE.reverse

-- | Produces all the prefixes of a stream, starting with the shortest.
-- The result is 'NonEmpty' because the result always contains the empty list as
-- the first element.
--
-- >>> inits [1,2,3] 
-- [[], [1], [1,2], [1,2,3]]
--
-- >>> inits [1]
-- [[], [1]]
--
-- >>> inits []
-- [[]]
inits :: Foldable f => f a -> NonEmpty [a] 
inits = unsafeFromList . List.inits . Foldable.toList


-- | Produces all the nonempty prefixes of a nonempty stream, starting with the shortest.
--
-- >>> inits1 [1,2,3]
-- [[1], [1,2], [1,2,3]]
--
-- >>> inits1 [1]
-- [[1]]
inits1 :: forall a. NonEmpty a -> NonEmpty (NonEmpty a) 
# if (MIN_VERSION_base(4,18,0)) || (MIN_VERSION_base_compat(0,13,1))
inits1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.inits1
#else
inits1 = unsafeFromList . List.map unsafeFromList . List.tail . List.inits . toList
# endif

-- | Produces all the suffixes of a stream, starting with the longest.
-- The result is 'NonEmpty' because the result always contains the empty list as
-- the first element.
--
-- >>> tails [1,2,3]
-- [[1, 2, 3], [2, 3], [3], []]
--
-- >>> tails [1]
-- [[1], []]
--
-- >>> tails []
-- [[]]
tails :: Foldable f => f a -> NonEmpty [a]
tails = fromNonEmpty . NE.tails

-- | Produces all the nonempty suffixes of a nonempty stream, starting with the longest.
--
-- >>> tails1 [1,2,3]
-- [[1, 2, 3], [2, 3], [3]]
--
-- >>> tails1 [1]
-- [[1]]
tails1 :: forall a. NonEmpty a -> NonEmpty (NonEmpty a) 
# if (MIN_VERSION_base(4,18,0)) || (MIN_VERSION_base_compat(0,13,1))
tails1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.tails1
#else
tails1 = unsafeFromList . List.map unsafeFromList . List.init . List.tails . toList
#endif

-- | A monomorphic version of <> for 'NonEmpty'.
--
-- >>> append [1] [2, 3]
-- [1, 2, 3]
append :: NonEmpty a -> NonEmpty a -> NonEmpty a 
append (NonEmpty (x NE.:| xs)) (NonEmpty (y NE.:| ys))
  = NonEmpty $ x NE.:| (xs ++ y : ys)

-- | Append a list at the end of a 'NonEmpty'.
--
-- >>> appendList [1, 2, 3] []
-- [1, 2, 3]
--
-- >>> appendList [1, 2, 3] [4, 5]
-- [1, 2, 3, 4, 5]
appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (NonEmpty (x NE.:| xs)) ys
  = NonEmpty $ x NE.:| (xs ++ ys)

-- | Prepend a list to the front of a 'NonEmpty'.
--
-- >>> prependList [] [1, 2, 3]
-- [1, 2, 3]
--
-- >>> prependList [negate 1, 0] [1, 2, 3]
-- [-1, 0, 1, 2, 3]
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList [] xs = xs
prependList (x : xs) (NonEmpty (y NE.:| ys))
  = NonEmpty $ x NE.:| (xs ++ y : ys)


-- * Stream transformations

-- | Map a function over a NonEmpty stream.
map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f = onUnderlying (NE.map f)

-- | Produces a 'NonEmpty' which alternates between elementes of
-- the input list, and the supplied element.
--
-- >>> intersperse 0 [1, 2, 3])
-- [1, 0, 2, 0, 3]
--
-- >>> intersperse 0 [1]
-- [1]
intersperse :: a -> NonEmpty a -> NonEmpty a 
intersperse el = onUnderlying (NE.intersperse el)

-- | Left-associative fold, lazy in the accumulator. See 'List.foldl'.
foldl1 :: (a -> a -> a) -> NonEmpty a -> a
foldl1 f l = let (x, xs) = uncons l in
  List.foldl f x xs

-- | Left-associative fold, strict in the accumulator. See `List.foldl'`.
foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f l = let (x, xs) = uncons l in
  List.foldl' f x xs

-- | Left-associative fold, strict in the accumulator. See `List.foldl'`.
foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 f l = let (xs, x) = unsnoc l in
  List.foldr f x xs

-- | scanl is similar to foldl, but returns a stream of successive
-- reduced values from the left:
--
-- >>> scanl (+) 1 [20, 300, 4000]
-- [1,21,321,4321]
scanl :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b 
scanl f initial els = fromNonEmpty $ NE.scanl f initial (Foldable.toList els)

-- | A strict version of 'scanl'.
scanl' :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b 
scanl' f initial els = unsafeFromList $ List.scanl' f initial (Foldable.toList els)

-- | scanl1 is a variant of scanl that has no starting value argument:
--
-- @
-- scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, x1 `f` (x2 `f` x3), ...]
-- @
scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 f = onUnderlying (NE.scanl1 f)

-- | Right-to-left dual of scanl. Note that the order of parameters
-- on the accumulating function are reversed compared to scanl.
-- Also note that
--
-- @
-- head (scanr f z xs) == foldr f z xs
-- @
--
-- >>> scanr (+) 0 [1..4]
-- [10,9,7,4,0]
--
-- >>> scanr (+) 42 []
-- [42]
--
-- >>> scanr (-) 100 [1..4]
-- [98,-97,99,-96,100]
scanr :: Foldable f => (a -> b -> b) -> b -> f a -> NonEmpty b 
scanr f initial els = fromNonEmpty $ NE.scanr f initial els

-- | scanr1 is a variant of scanr that has no starting value argument.
--
-- >>> scanr1 (+) [1..4]
-- [10,9,7,4]
--
-- >>> scanr1 (+) []
-- []
--
-- >>> scanr1 (-) [1..4]
-- [-2,3,-1,4]
scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a 
scanr1 f = onUnderlying (NE.scanr1 f)

-- | transpose for NonEmpty, behaves the same as 'List.transpose'.
-- The rows/columns need not be the same length, in which case
--
-- @
-- transpose . transpose /= id
-- @
transpose :: forall a. NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.transpose

-- | Behaves the same as 'List.sortBy'
sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy f = onUnderlying (NE.sortBy f)

-- | Sort a list on a projection of its elements.
-- Projects once, then sorts, then un-projects.
-- This is useful when the projection function is expensive.
-- If it's not, you should probably use 'sortWith'.
sortOn :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a 
sortOn f = onList (List.sortOn f)

-- | Sort a list on a projection of its elements.
-- Projects during comparison.
-- This is useful when the projection function is cheap.
-- If it's not, you should probably use 'sortOn'.
sortWith :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a 
sortWith f = onUnderlying (NE.sortWith f)


-- * Building streams

-- | iterate f x produces the infinite sequence of repeated applications of f to x.
--
-- @
-- iterate f x = [x, f x, f (f x), ..]
-- @
iterate :: (a -> a) -> a -> NonEmpty a
iterate f initial = fromNonEmpty $ NE.iterate f initial

-- | repeat x returns a constant stream, where all elements are equal to x.
repeat :: a -> NonEmpty a
repeat = fromNonEmpty . NE.repeat

-- | cycle xs returns the infinite repetition of xs:
--
-- @
-- cycle [1, 2, 3] == [1, 2, 3, 1, 2, 3, ...]
-- @
cycle :: NonEmpty a -> NonEmpty a
cycle = onNonEmpty NE.cycle

-- | insert x xs inserts x into the last position in xs where it is still
-- less than or equal to the next element. If the list is sorted beforehand,
-- the result will also be sorted.
insert :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert el = unsafeFromList . List.insert el . Foldable.toList

-- | some1 x sequences x one or more times.
some1 :: Alternative f => f a -> f (NonEmpty a)
some1 = fmap fromNonEmpty . NE.some1


-- * Extracting sublists

-- | take n xs returns the first n elements of xs.
take :: Int -> NonEmpty a -> [a]
take n = NE.take n . toNonEmpty

-- | drop n xs drops the first n elements from the front of the sequence xs.
drop :: Int -> NonEmpty a -> [a]
drop n = NE.drop n . toNonEmpty

-- | splitAt n xs returns a pair consisting of the prefix of xs of length n and the remaining stream immediately following this prefix.
--
-- 'splitAt' n xs == ('take' n xs, 'drop' n xs)
splitAt :: Int -> NonEmpty a -> ([a], [a])
splitAt n = NE.splitAt n . toNonEmpty

-- | Produces the longest prefix of the stream for which the predicate holds.
takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile f = NE.takeWhile f . toNonEmpty

-- | dropWhile p xs produces the suffix remaining after takeWhile p xs.
dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile f = NE.dropWhile f . toNonEmpty

-- | span p xs returns the longest prefix of xs that satisfies p, together with the remainder of the stream.
--
-- 'span' p xs == ('takeWhile' p xs, 'dropWhile' p xs)
span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span f = NE.span f . toNonEmpty

-- | break p is equivalent to span (not . p).
break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break f = NE.break f . toNonEmpty

-- | Removes any elements of a nonempty stream that do not satisfy a predicate.
filter :: (a -> Bool) -> NonEmpty a -> [a]
filter f = NE.filter f . toNonEmpty

-- | Produces a pair of lists, the first of elements that satisfy the given
-- predicate, the second of elements that did not.
--
-- @
-- 'partition' p xs == ('filter' p xs, 'filter' (not . p) xs)
-- @
partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition f = NE.partition f . toNonEmpty

-- | Takes a stream and returns a list of streams such that flattening
-- the resulting list is equal to the argument.
-- Moreover, each stream in the resulting list contains only equal elements.
--
-- >>> group "Mississippi"
-- ["M", "i", "ss", "i", "ss", "i", "pp", "i"]
group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group = groupBy (==)

-- | Similar to 'group', but sorts the input first so that each
-- equivalence class has, at most, one list in the output.
groupAll :: Ord a => [a] -> [NonEmpty a]
groupAll = groupAllBy compare

-- | Similar to 'group', but uses the provided equality predicate instead of '=='.
groupBy :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy f = Exts.coerce . NE.groupBy f

ordToEq :: (a -> b -> Ordering) -> a -> b -> Bool
ordToEq f a b = f a b == EQ

-- | Similar to 'groupBy', but sorts the input first so that each
-- equivalence class has, at most, one list in the output.
groupAllBy :: (a -> a -> Ordering) -> [a] -> [NonEmpty a]
groupAllBy f = groupBy (ordToEq f) . List.sortBy f

-- | Similar to 'group', but uses the provided projection when comparing for equality.
groupWith :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
groupWith f = Exts.coerce . NE.groupWith f

-- | Similar to 'groupWith', but sorts the input first so that each
-- equivalence class has, at most, one list in the output.
groupAllWith :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith f = Exts.coerce . NE.groupAllWith f

-- | Similar to 'List.group', but uses the knowledge that its input is non-empty to
-- produce guaranteed non-empty output.
group1 :: forall a. Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.group1

-- | Similar to 'group1', but sorts the input first so that each
-- equivalence class has, at most, one list in the output.
groupAll1 :: Ord a => NonEmpty a -> [NonEmpty a]
groupAll1 = group . sort

-- | Similar to 'group1', but uses the provided equality predicate instead of '=='.
groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 f = onUnderlying (NE.groupBy1 f)

-- | Similar to 'group', but sorts the input first so that each
-- equivalence class has, at most, one list in the output.
groupAllBy1 :: (a -> a -> Ordering) -> [a] -> [NonEmpty a]
groupAllBy1 f = groupBy (ordToEq f) . List.sortBy f

-- | Similar to 'group1', but uses the provided projection when comparing for equality.
groupWith1 :: Eq b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith1 f = onUnderlying (NE.groupWith1 f)

-- | Similar to 'groupWith1', but sorts the list first so that each
-- equivalence class has, at most, one list in the output.
groupAllWith1 :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith1 f = onUnderlying (NE.groupAllWith1 f)


-- * Sublist predicates

-- | Returns True if the first argument is a prefix of the second.
--
-- >>> isPrefixOf [1, 2, 3] [1, 2, 3, 4, 5]
-- True
-- >>> isPrefixOf "abc" "defghi"
-- False
-- >>> isPrefixOf "abc" ""
-- False
isPrefixOf :: Eq a => [a] -> NonEmpty a -> Bool
isPrefixOf els = NE.isPrefixOf els . toNonEmpty


-- * "Set" operations

-- | Removes duplicate elements from a list. In particular, it keeps only
-- the first occurrence of each element. (The name nub means 'essence'.)
-- It is a special case of nubBy, which allows the programmer to supply
-- their own inequality test.
nub :: Eq a => NonEmpty a -> NonEmpty a 
nub = onNonEmpty NE.nub

-- | Behaves just like nub, except it uses a user-supplied equality
-- predicate instead of the overloaded '==' function.
nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a 
nubBy f = onNonEmpty (NE.nubBy f)


-- * Indexing streams

infixl 9 !?

-- | xs !! n returns the element of the stream xs at index n, if present.
-- Note that the head of the stream has index 0.
(!?) :: NonEmpty a -> Int -> Maybe a
(!?) l n
  | n < 0 = Nothing
  | otherwise = listToMaybe $ List.drop n $ toList l

-- * Zipping streams

-- | \(\mathcal{O}(\min(m,n))\).
-- Takes two streams and produces a stream of corresponding pairs.
--
-- >>> zip [1, 2] ['a', 'b']
-- [(1, 'a'),(2, 'b')]
zip :: forall a b. NonEmpty a -> NonEmpty b -> NonEmpty (a, b) 
zip l1 l2 = Exts.coerce @(NE.NonEmpty (a, b)) $ NE.zip (Exts.coerce l1) (Exts.coerce l2)

-- | \(\mathcal{O}(\min(m,n))\).
-- Generalises zip by zipping with the provided function, instead of
-- a tupling function.
zipWith :: forall a b c. (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c 
zipWith f l1 l2 = Exts.coerce @(NE.NonEmpty c) $ NE.zipWith f (Exts.coerce l1) (Exts.coerce l2)
