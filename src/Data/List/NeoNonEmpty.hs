{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE RankNTypes #-}

{-|
NonEmpty - Like base's NonEmpty but with:
* Show and Read instance similar to []
* A completely safe API
* Extra utils

Extra functions:
* aNonEmpty
* scanl'
-}

module Data.List.NeoNonEmpty
  (
  -- * Construction
    NonEmpty(..)
  , fromCons

  -- * Type annotations
  , aNonEmpty

  -- * Converting to and from base's 'NE.NonEmpty'
  , fromNonEmpty
  , toNonEmpty

  -- * Converting to and from lists
  , fromList
  , toList

  -- * Stream transformations
  , map
  , intersperse
  , scanl
  , scanl'
  ) where

import Prelude
  ( Applicative(..)
  , Eq
  , Foldable
  , Functor(..)
  , Int
  , Maybe(..)
  , Monad(..)
  , Ord
  , Ordering
  , Semigroup
  , Show(..)
  , ($)
  , (.)
  , fail
  )

import GHC.Generics                    (Generic, Generic1)
import Control.Monad.Fix               (MonadFix)
import Control.Monad.Zip               (MonadZip)
import Data.Bifunctor                  (second)
import Data.Data                       (Data)
import Data.Maybe                      (fromJust)
import Data.Foldable1.Compat           (Foldable1)
import Data.Functor.Classes            (Eq1, Ord1, Read1, Show1)
import Text.Read                       (Read(..))

import qualified GHC.Exts                  as Exts
import qualified Data.Foldable             as F
import qualified Data.List                 as L
import qualified Data.List.NonEmpty.Compat as NE


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

-- | Make a 'NonEmpty' From an element and a list
fromCons :: a -> [a] -> NonEmpty a
fromCons x xs = NonEmpty (x NE.:| xs)


-- * Refining

-- | A non empty thing. Useful as a syntactically lightweight type annotation,
-- especially when using OverloadedLists
--
-- @
-- >>> [(), ()]
--     • Ambiguous type variable ‘a0’ arising from a use of ‘print’
--       prevents the constraint ‘(Show a0)’ from being solved.
-- >>> aNonEmpty [(), ()]
-- [(),()]
-- @
aNonEmpty :: NonEmpty a -> NonEmpty a
aNonEmpty a = a


-- * Converting to and from nonempties

-- Converts base's 'NE.NonEmpty' to a 'NonEmpty'
fromNonEmpty :: NE.NonEmpty a -> NonEmpty a
fromNonEmpty = NonEmpty

-- Converts a 'NonEmpty' to base's 'NonEmpty'
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


-- * Stream transformations

-- Unsafe. Not exported.
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


map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f = onUnderlying (NE.map f)

intersperse :: a -> NonEmpty a -> NonEmpty a 
intersperse el = onUnderlying (NE.intersperse el)

scanl :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b 
scanl f initial els = fromNonEmpty $ NE.scanl f initial els

scanl' :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b 
scanl' f initial els = unsafeFromList $ L.scanl' f initial (F.toList els)

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

sortWith :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a 
sortWith f = onUnderlying (NE.sortWith f)

-- * Basic functions

length :: NonEmpty a -> Int
length = NE.length . toNonEmpty

head :: NonEmpty a -> a
head = NE.head . toNonEmpty

tail :: NonEmpty a -> [a]
tail = NE.tail . toNonEmpty

last :: NonEmpty a -> a
last = NE.last . toNonEmpty

init :: NonEmpty a -> [a]
init = NE.init . toNonEmpty

singleton :: a -> NonEmpty a
singleton a = NonEmpty (a NE.:| [])

infixr 5 <|
(<|) :: a -> NonEmpty a -> NonEmpty a
(<|) = cons

cons :: a -> NonEmpty a -> NonEmpty a
cons el = onUnderlying (NE.cons el)

uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons = second Exts.coerce . NE.uncons . toNonEmpty

unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr f initial = fromNonEmpty $ NE.unfoldr f initial

sort :: forall a. Ord a => NonEmpty a -> NonEmpty a
sort = onNonEmpty NE.sort

reverse :: NonEmpty a -> NonEmpty a
reverse = onNonEmpty NE.reverse

inits :: Foldable f => f a -> NonEmpty [a] 
inits = fromNonEmpty . NE.inits

inits1 :: forall a. NonEmpty a -> NonEmpty (NonEmpty a) 
inits1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.inits1

tails :: Foldable f => f a -> NonEmpty [a]
tails = fromNonEmpty . NE.tails

tails1 :: forall a. NonEmpty a -> NonEmpty (NonEmpty a) 
tails1 = onUnderlying @_ @(NE.NonEmpty (NE.NonEmpty a)) NE.tails1


-- * Building streams

iterate :: (a -> a) -> a -> NonEmpty a
repeat :: a -> NonEmpty a
cycle :: NonEmpty a -> NonEmpty a
unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
insert :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
some1 :: Alternative f => f a -> f (NonEmpty a)
