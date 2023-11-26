{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main
  ( main
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Read1(..), Show1(..))
import Hedgehog             (Group(..), Property, checkParallel, property, forAll, (===), Gen)

import qualified Data.List.NeoNonEmpty as NNE
import qualified Data.List.NonEmpty    as NE

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified System.Exit    as System

main :: IO ()
main = do
  res <- checkParallel tests
  System.exitWith $ if res then System.ExitSuccess else System.ExitFailure 1

genNonEmpty :: Gen (NNE.NonEmpty Char)
genNonEmpty = NNE.fromNonEmpty <$> Gen.nonEmpty (Range.linear 0 100) Gen.alpha

againstNonEmpty :: (Show b, Eq b) => (NE.NonEmpty Char -> b) -> (NNE.NonEmpty Char -> b) -> Property
againstNonEmpty f g = property $ do
  xs <- forAll genNonEmpty
  f (NNE.toNonEmpty xs) === g xs

snocOne :: Property
snocOne = property $ do
  x <- forAll Gen.alpha
  NNE.unsnoc (NNE.singleton x) === ([], x)

snocUnsnoc :: Property
snocUnsnoc = property $ do
  x <- forAll Gen.alpha
  xs <- forAll genNonEmpty
  NNE.unsnoc (NNE.snoc xs x) === (NNE.toList xs, x)

singletonRoundtrips :: Property
singletonRoundtrips = property $ do
  do
    x <- forAll $ Gen.int $ Range.linear (-25) 25
    x NE.:| [] === NNE.toNonEmpty (NNE.singleton x)
  do
    x <- forAll $ Gen.int $ Range.linear (-25) 25
    NNE.fromNonEmpty (x NE.:| []) === NNE.singleton x

consFromCons :: Property
consFromCons = property $ do
  x <- forAll Gen.alpha
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  (x NE.:| xs) === NNE.toNonEmpty (NNE.fromCons x xs)

aNonEmptyIsId :: Property
aNonEmptyIsId = property $ do
  xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
  let xs' = NNE.fromNonEmpty xs
  NNE.aNonEmpty xs' === xs'

consProp :: Property
consProp = property $ do
  x <- forAll Gen.alpha
  xs <- forAll genNonEmpty
  NNE.toList (NNE.cons x xs) === NE.toList (x NE.:| NNE.toList xs)
  NNE.toList (x NNE.:| NNE.toList xs) === NE.toList (x NE.:| NNE.toList xs)
  let x' NNE.:| xs' = xs
  x' : xs' === NNE.toList xs

unConsProp :: Property
unConsProp = property $ do
  x <- forAll Gen.alpha
  xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
  NNE.toList (NNE.cons x $ NNE.fromNonEmpty xs) === NE.toList (x NE.:| NE.toList xs)

indexProp :: Property
indexProp = property $ do
  ind <- forAll $ Gen.int $ Range.linear 0 10
  xs <- forAll genNonEmpty
  case xs NNE.!? ind of
    Just a ->  a === (NNE.toList xs !! ind)
    Nothing -> pure ()

eqProp :: Property
eqProp = property $ do
  do
    xs <- forAll genNonEmpty
    ys <- forAll genNonEmpty
    (xs == ys) === liftEq (==) xs ys
    (xs == ys) === (NNE.toList xs == NNE.toList ys)
  do
    xs <- forAll genNonEmpty
    let ys = NNE.snoc xs 'a'
    (xs == ys) === liftEq (==) xs ys
    (xs == ys) === (NNE.toList xs == NNE.toList ys)

ordProp :: Property
ordProp = property $ do
  do
    xs <- forAll genNonEmpty
    ys <- forAll genNonEmpty
    compare xs ys === liftCompare compare xs ys
    compare xs ys === compare (NNE.toList xs) (NNE.toList ys)
  do
    xs <- forAll genNonEmpty
    let ys = NNE.snoc xs 'a'
    compare xs ys === liftCompare compare xs ys
    compare xs ys === compare (NNE.toList xs) (NNE.toList ys)

tests :: Group
tests = Group "hedgehog properties"
  [ ("length", againstNonEmpty NE.length NNE.length)
  , ("roundtrips singleton", singletonRoundtrips)
  , ("NE.:| === NNE.fromCons forwards", consFromCons)
  , ("aNonEmpty === id", aNonEmptyIsId)
  , ("head", againstNonEmpty NE.head NNE.head)
  , ("tail", againstNonEmpty NE.tail NNE.tail)
  , ("last", againstNonEmpty NE.last NNE.last)
  , ("init", againstNonEmpty NE.init NNE.init)
  , ("cons", consProp)
  , ("uncons", unConsProp)
  , ("snocOne", snocOne)
  , ("snoc/unsnoc", snocUnsnoc)
  , ("(!?)", indexProp)
  , ("Eq", eqProp)
  , ("Ord", ordProp)
  ]
