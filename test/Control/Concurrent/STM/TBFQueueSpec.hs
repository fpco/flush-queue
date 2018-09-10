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

prop_FillReadTakeNonBlocking :: NonEmptyList Int -> Property
prop_FillReadTakeNonBlocking (NonEmpty ls) = monadicIO $ do
  run $ do
    let x:xs = ls
        i = length xs
    q <- newTBFQueueIO (i + 1)
    mapM_ (atomically . writeTBFQueue q) (x:xs)
    x' <- atomically $ readTBFQueue q
    xs' <- atomically $ takeTBFQueue i q
    return (x === x' .&&. xs === xs')


prop_FillReadTakeBlocking :: NonEmptyList Int -> Int -> Property
prop_FillReadTakeBlocking (NonEmpty ls) y = monadicIO $ do
  run $ do
    let x:xs = ls
        i = length xs
    q <- newTBFQueueIO (i + 1)
    mapM_ (atomically . writeTBFQueue q) (x:xs)
    ((), x') <- concurrently (atomically $ writeTBFQueue q y) (atomically $ readTBFQueue q)
    xs' <- atomically $ takeTBFQueue i q
    y' <- atomically $ readTBFQueue q
    return (x === x' .&&. xs === xs' .&&. y === y')


prop_PushPopConcurrently :: Int -> Positive Int -> Property
prop_PushPopConcurrently x (Positive bound) = monadicIO $ do
  run $ do
    q <- newTBFQueueIO bound
    (x', ()) <- concurrently (atomically $ readTBFQueue q) (atomically $ writeTBFQueue q x)
    return (x === x')


spec :: Spec
spec = do
  describe "Fill+Flush" $ do
    it "FillFlushNonBlocking" $ property prop_FillFlushNonBlocking
    it "FillAndBlockFlush" $ property prop_FillAndBlockFlush
    it "FillReadTakeNonBlocking" $ property prop_FillReadTakeNonBlocking
    it "FillReadTakeBlocking" $ property prop_FillReadTakeBlocking
    it "PushPopConcurrently" $ property prop_PushPopConcurrently
