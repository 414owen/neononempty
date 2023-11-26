{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import Hedgehog (Property, property, forAll, (===), Gen)
import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Data.List.NeoNonEmpty as NNE
import qualified Data.List.NonEmpty    as NE

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Test.Tasty.Hedgehog as H

main :: IO ()
main = defaultMain tests

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

tests :: TestTree
tests = testGroup "hedgehog properties"
  [ H.testProperty "length" $ againstNonEmpty NE.length NNE.length
  , H.testProperty "roundtrips singleton forwards" $ property $ do
      x <- forAll $ Gen.int $ Range.linear (-25) 25
      x NE.:| [] === NNE.toNonEmpty (NNE.singleton x)
  , H.testProperty "roundtrips singleton backwards" $ property $ do
      x <- forAll $ Gen.int $ Range.linear (-25) 25
      NNE.fromNonEmpty (x NE.:| []) === NNE.singleton x
  , H.testProperty "NE.:| === NNE.fromCons forwards" $ property $ do
      x <- forAll Gen.alpha
      xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
      (x NE.:| xs) === NNE.toNonEmpty (NNE.fromCons x xs)
  , H.testProperty "aNonEmpty === id" $ property $ do
      xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
      let xs' = NNE.fromNonEmpty xs
      NNE.aNonEmpty xs' === xs'
  , H.testProperty "head" $ againstNonEmpty NE.head NNE.head
  , H.testProperty "tail" $ againstNonEmpty NE.tail NNE.tail
  , H.testProperty "last" $ againstNonEmpty NE.last NNE.last
  , H.testProperty "init" $ againstNonEmpty NE.init NNE.init
  , H.testProperty "cons" $ property $ do
      x <- forAll Gen.alpha
      xs <- forAll genNonEmpty
      NNE.toList (NNE.cons x xs) === NE.toList (x NE.:| NNE.toList xs)
      NNE.toList (x NNE.:| NNE.toList xs) === NE.toList (x NE.:| NNE.toList xs)
      let x' NNE.:| xs' = xs
      let y' : ys = NNE.toList xs
      x' === y'
      xs' === ys
  , H.testProperty "uncons" $ property $ do
      x <- forAll Gen.alpha
      xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
      NNE.toList (NNE.cons x $ NNE.fromNonEmpty xs) === NE.toList (x NE.:| NE.toList xs)
  , H.testProperty "snocOne" snocOne
  , H.testProperty "snoc/unsnoc" snocUnsnoc
  , H.testProperty "(!?)" $ property $ do
      ind <- forAll $ Gen.int $ Range.linear 0 10
      xs <- forAll genNonEmpty
      case xs NNE.!? ind of
        Just a ->  a === (NNE.toList xs !! ind)
        Nothing -> pure ()
  ]
