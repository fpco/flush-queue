module Control.Concurrent.BFQueueSpec (spec) where

import Control.Concurrent.Async
import Control.Concurrent.BFQueue
import Data.List

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic


prop_FillFlushNonBlocking :: [[Int]] -> Property
prop_FillFlushNonBlocking lss = monadicIO $ do
  ls <- run $ do
    q <- newBFQueue (sum (map length lss))
    mapConcurrently_ (foldMap (writeBFQueue q)) lss
    flushBFQueue q
  return (sort ls === sort (concat lss))

prop_FillAndBlockFlush :: Positive Int -> [Int] -> Int -> Property
prop_FillAndBlockFlush (Positive bound) ls oneExtra =
  bound < length ls ==> monadicIO $ do
    let (fillWith, leftOver) = splitAt bound ls
    run $ do
      q <- newBFQueue bound
      isSuccess <- and <$> mapConcurrently (tryWriteBFQueue q) fillWith
      hasSpace <- or <$> mapConcurrently (tryWriteBFQueue q) leftOver
      len <- lengthBFQueue q
      eLs <- race (writeBFQueue q oneExtra >> flushBFQueue q) (flushBFQueue q)
      return $
        conjoin
          [ counterexample "Queue wasn't fully filled up" isSuccess
          , counterexample "Left over was placed on the queue" (not hasSpace)
          , len === length fillWith
          , either
              (\o ->
                 o === [oneExtra] .||.
                 counterexample "Placed an element on the full queue concurrently" False)
              (\ls' -> sort ls' === sort fillWith)
              eLs
          ]


spec :: Spec
spec = do
  describe "Fill+Flush" $ do
    it "FillFlushNonBlocking" $ property prop_FillFlushNonBlocking
    it "FillAndBlockFlush" $ property prop_FillAndBlockFlush
