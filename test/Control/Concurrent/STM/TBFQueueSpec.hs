module Control.Concurrent.STM.TBFQueueSpec (spec) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBFQueue
import Data.List

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_FillFlushNonBlocking :: [[Int]] -> Property
prop_FillFlushNonBlocking lss = monadicIO $ do
  ls <- run $ do
    q <- newTBFQueueIO (sum (map length lss))
    mapConcurrently_ (foldMap (atomically . writeTBFQueue q)) lss
    atomically $ flushTBFQueue q
  return (sort ls === sort (concat lss))

prop_FillAndBlockFlush :: Positive Int -> [Int] -> Int -> Property
prop_FillAndBlockFlush (Positive bound) ls oneExtra =
  bound < length ls ==> monadicIO $ do
    let (fillWith, leftOver) = splitAt bound ls
    run $ do
      q <- atomically $ newTBFQueue bound
      isSuccess <- and <$> mapConcurrently (atomically . tryWriteTBFQueue q) fillWith
      hasSpace <- or <$> mapConcurrently (atomically . tryWriteTBFQueue q) leftOver
      len <- atomically $ lengthTBFQueue q
      eLs <- race (atomically $ writeTBFQueue q oneExtra) (atomically $ flushTBFQueue q)
      return $
        conjoin
          [ counterexample "Queue wasn't fully filled up" isSuccess
          , counterexample "Left over was placed on the queue" (not hasSpace)
          , len === length fillWith
          , either
              (\_ -> counterexample "Placed an element on the full queue concurrently" False)
              (\ls' -> sort ls' === sort fillWith) eLs
          ]


spec :: Spec
spec = do
  describe "Fill+Flush" $ do
    it "FillFlushNonBlocking" $ property prop_FillFlushNonBlocking
    it "FillAndBlockFlush" $ property prop_FillAndBlockFlush
