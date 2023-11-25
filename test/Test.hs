{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import Hedgehog (Property, property, forAll, (===))
import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Data.List.NeoNonEmpty as NNE
import qualified Data.List.NonEmpty    as NE

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Test.Tasty.Hedgehog as H

main :: IO ()
main = defaultMain tests

againstNonEmpty :: (Show b, Eq b) => (NE.NonEmpty Char -> b) -> (NNE.NonEmpty Char -> b) -> Property
againstNonEmpty f g = property $ do
  xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
  f xs === g (NNE.fromNonEmpty xs)

tests :: TestTree
tests = testGroup "hedgehog properties"
  [ H.testProperty "length" $ againstNonEmpty NE.length NNE.length
  , H.testProperty "roundtrips singleton forwards" $ property $ do
      x <- forAll $ Gen.int $ Range.linear (-25) 25
      NE.singleton x === NNE.toNonEmpty (NNE.singleton x)
  , H.testProperty "roundtrips singleton backwards" $ property $ do
      x <- forAll $ Gen.int $ Range.linear (-25) 25
      NNE.fromNonEmpty (NE.singleton x) === NNE.singleton x
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
      xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
      NNE.toList (NNE.cons x $ NNE.fromNonEmpty xs) === NE.toList (x NE.:| NE.toList xs)
  , H.testProperty "uncons" $ property $ do
      x <- forAll Gen.alpha
      xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
      NNE.toList (NNE.cons x $ NNE.fromNonEmpty xs) === NE.toList (x NE.:| NE.toList xs)
  ]
