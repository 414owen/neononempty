{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE RankNTypes          #-}

{-|
NonEmpty - Like base's NonEmpty but with:

  * 'Show' and 'Read' instance similar to `[]`
  * A completely safe API
  * Extra utils


Added functions:

  * 'aNonEmpty'
  * `scanl'`
  * 'sortOn'
  * 'fromCons'


Removed functions:

  * 'NE.unzip'  (not nonempty-specific)
  * 'NE.unfold' (deprecated)
  * 'NE.:|'     (use fromCons)


Replaced functions:

  * s/(!!)/(!?)/
-}

module Data.List.NeoNonEmpty
  (
  -- * Construction
    NonEmpty(..)
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
  , scanl
  , scanl'
  , scanr
  , scanl1
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
  , groupBy
  , groupWith
  , groupAllWith
  , group1
  , groupBy1
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
  , Ordering
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
import Data.Bifunctor                  (second)
import Data.Data                       (Data)
import Data.Foldable1.Compat           (Foldable1)
import Data.Functor.Classes            (Eq1, Ord1, Read1, Show1)
import Data.Maybe                      (fromJust, listToMaybe)
import GHC.Generics                    (Generic, Generic1)
import Text.Read                       (Read(..))

import qualified Data.Foldable             as Foldable
import qualified Data.List                 as List
import qualified Data.List.NonEmpty.Compat as NE
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

newtype NonEmpty a = NonEmpty (NE.NonEmpty a)
  deriving (Generic, Generic1, Data)
  deriving
    ( Applicative
    , Functor
    , MonadFix
    , MonadZip
    , Foldable
    , Foldable1
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
-- and a stream of the remaining elements, if any.
uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons = second Exts.coerce . NE.uncons . toNonEmpty

-- | Dual of 'foldr', see 'List.unfoldr'.
unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr f initial = fromNonEmpty $ NE.unfoldr f initial

-- | Sort a nonempty stream.
sort :: forall a. Ord a => NonEmpty a -> NonEmpty a
sort = onNonEmpty NE.sort

-- | Reverse a nonempty stream.
reverse :: NonEmpty a -> NonEmpty a
reverse = onNonEmpty NE.reverse

-- | Produces all the prefixes of a nonempty stream, starting with the shortest.
-- The result is 'NonEmpty' because the result always contains the empty list as
-- the first element.
--
-- >>> inits [1,2,3] 
-- [[], [1], [1,2], [1,2,3]]
-- >>> inits [1]
-- [[], [1]]
-- >>> inits []
-- [[]]
inits :: Foldable f => f a -> NonEmpty [a] 
inits = unsafeFromList . List.inits . Foldable.toList

-- | 
inits1 :: forall a. NonEmpty a -> NonEmpty (NonEmpty a) 
inits1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.inits1

tails :: Foldable f => f a -> NonEmpty [a]
tails = fromNonEmpty . NE.tails

tails1 :: forall a. NonEmpty a -> NonEmpty (NonEmpty a) 
tails1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.tails1

-- base-compat added these a little late.
append :: NonEmpty a -> NonEmpty a -> NonEmpty a 
append (NonEmpty (x NE.:| xs)) (NonEmpty (y NE.:| ys)) = NonEmpty $ x NE.:| (xs ++ y : ys)

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (NonEmpty (x NE.:| xs)) ys = NonEmpty $ x NE.:| (xs ++ ys)

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList [] xs = xs
prependList (x : xs) (NonEmpty (y NE.:| ys)) = NonEmpty $ x NE.:| (xs ++ y : ys)


-- * Stream transformations

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f = onUnderlying (NE.map f)

intersperse :: a -> NonEmpty a -> NonEmpty a 
intersperse el = onUnderlying (NE.intersperse el)

scanl :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b 
scanl f initial els = fromNonEmpty $ NE.scanl f initial (Foldable.toList els)

scanl' :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b 
scanl' f initial els = unsafeFromList $ List.scanl' f initial (Foldable.toList els)

scanr :: Foldable f => (a -> b -> b) -> b -> f a -> NonEmpty b 
scanr f initial els = fromNonEmpty $ NE.scanr f initial els

scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 f = onUnderlying (NE.scanl1 f)

scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a 
scanr1 f = onUnderlying (NE.scanr1 f)

transpose :: forall a. NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.transpose

sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy f = onUnderlying (NE.sortBy f)

sortOn :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a 
sortOn f = onList (List.sortOn f)

sortWith :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a 
sortWith f = onUnderlying (NE.sortWith f)


-- * Building streams

iterate :: (a -> a) -> a -> NonEmpty a
iterate f initial = fromNonEmpty $ NE.iterate f initial

repeat :: a -> NonEmpty a
repeat = fromNonEmpty . NE.repeat

cycle :: NonEmpty a -> NonEmpty a
cycle = onNonEmpty NE.cycle

insert :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert el = unsafeFromList . List.insert el . Foldable.toList

some1 ::
  ( Exts.Coercible (f (NE.NonEmpty a)) (f (NonEmpty a))
  , Alternative f
  ) => f a -> f (NonEmpty a)
some1 = Exts.coerce . NE.some1


-- * Extracting sublists

take :: Int -> NonEmpty a -> [a]
take n = NE.take n . toNonEmpty

drop :: Int -> NonEmpty a -> [a]
drop n = NE.drop n . toNonEmpty

splitAt :: Int -> NonEmpty a -> ([a], [a])
splitAt n = NE.splitAt n . toNonEmpty

takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile f = NE.takeWhile f . toNonEmpty

dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile f = NE.dropWhile f . toNonEmpty

span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span f = NE.span f . toNonEmpty

break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break f = NE.break f . toNonEmpty

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter f = NE.filter f . toNonEmpty

partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition f = NE.partition f . toNonEmpty

group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group = groupBy (==)

groupBy :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy f = Exts.coerce . NE.groupBy f

groupWith :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
groupWith f = Exts.coerce . NE.groupWith f

groupAllWith :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith f = Exts.coerce . NE.groupAllWith f

group1 :: forall a. Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.group1

groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 f = onUnderlying (NE.groupBy1 f)

groupWith1 :: Eq b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith1 f = onUnderlying (NE.groupWith1 f)

groupAllWith1 :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith1 f = onUnderlying (NE.groupAllWith1 f)


-- * Sublist predicates

isPrefixOf :: Eq a => [a] -> NonEmpty a -> Bool
isPrefixOf els = NE.isPrefixOf els . toNonEmpty


-- * "Set" operations

nub :: Eq a => NonEmpty a -> NonEmpty a 
nub = onNonEmpty NE.nub

nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a 
nubBy f = onNonEmpty (NE.nubBy f)


-- * Indexing streams

infixl 9 !?
(!?) :: NonEmpty a -> Int -> Maybe a
(!?) l n
  | n < 0 = Nothing
  | otherwise = listToMaybe (List.drop n (toList l))

-- * Zipping streams

zip :: forall a b. NonEmpty a -> NonEmpty b -> NonEmpty (a, b) 
zip l1 l2 = Exts.coerce @(NE.NonEmpty (a, b)) $ NE.zip (Exts.coerce l1) (Exts.coerce l2)

zipWith :: forall a b c. (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c 
zipWith f l1 l2 = Exts.coerce @(NE.NonEmpty c) $ NE.zipWith f (Exts.coerce l1) (Exts.coerce l2)
