-- |
-- Module      : Control.Concurrent.STM.TBFQueue
-- Copyright   : (c) FP Complete 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Transactional Bouded Flush Queue is a very similar to `Control.Concurrent.BFQueue.BFQueue`, with
-- an exception that it runs in `STM` and is also less efficient, but is still faster than
-- `Control.Concurrent.STM.TBQueue.TBQueue`.
--
module Control.Concurrent.STM.TBFQueue
  ( TBFQueue
  , newTBFQueue
  , newTBFQueueIO
  , writeTBFQueue
  , tryWriteTBFQueue
  , readTBFQueue
  , takeTBFQueue
  , flushTBFQueue
  , lengthTBFQueue
  ) where

import Control.Concurrent.BQueue
import Control.Concurrent.STM


-- | Bounded Flush Queue. It's a queue that allows pushing elements onto, popping elements from it,
-- but is mostly optimizied for flushing the queue or taking in bulk.
newtype TBFQueue a = TBFQueue (TVar (BQueue a))

-- | Construct a new empty Flush Bounded Queue
newTBFQueue :: Int -- ^ Maximum number of elements, that this queue can hold.
           -> STM (TBFQueue a)
newTBFQueue bound = TBFQueue <$> newTVar (newBQueue bound)


-- | Construct a new empty Flush Bounded Queue inside IO monad.
newTBFQueueIO :: Int -- ^ Maximum number of elements, that this queue can hold.
              -> IO (TBFQueue a)
newTBFQueueIO bound = TBFQueue <$> newTVarIO (newBQueue bound)

-- | /O(1)/ - Push an element onto the queue. Will block if maximum bound has been reached.
writeTBFQueue :: TBFQueue a -> a -> STM ()
writeTBFQueue (TBFQueue bQueueTVar) x = do
  bQueue <- readTVar bQueueTVar
  case pushBQueue x bQueue of
    Just newQueue -> writeTVar bQueueTVar newQueue
    Nothing -> retry

-- | /O(1)/ - Try to push an element onto the queue without blocking. Will return `True` if element
-- was pushed successfully, and `False` in case when the queue is full.
tryWriteTBFQueue :: TBFQueue a -> a -> STM Bool
tryWriteTBFQueue (TBFQueue bQueueTVar) x = do
  bQueue <- readTVar bQueueTVar
  case pushBQueue x bQueue of
    Just newQueue -> writeTVar bQueueTVar newQueue >> return True
    Nothing      -> return False

-- | /O(1)/ - Pop an element from the queue. Will block if queue is empty.
readTBFQueue :: TBFQueue a -> STM a
readTBFQueue (TBFQueue bQueueTVar) = do
  bQueue <- readTVar bQueueTVar
  case popBQueue bQueue of
    Just (x, newQueue) -> writeTVar bQueueTVar newQueue >> return x
    Nothing -> retry

-- | /O(n)/ - Flush the queue, unblock all the possible writers and return all the elements from the
-- queue in FIFO order.
flushTBFQueue :: TBFQueue a -> STM [a]
flushTBFQueue (TBFQueue bQueueTVar) = do
  bQueue <- readTVar bQueueTVar
  let (xs, newQueue) = flushBQueue bQueue
  writeTVar bQueueTVar newQueue
  return xs

-- | /O(i)/ - Take oldest @i@ elements from the queue. This function doesn't block and returns empty
-- list if queue is empty or @i@ isn't positive.
takeTBFQueue :: Int -> TBFQueue a -> STM [a]
takeTBFQueue i (TBFQueue bQueueTVar) = do
  bQueue <- readTVar bQueueTVar
  let (xs, newQueue) = takeBQueue i bQueue
  writeTVar bQueueTVar newQueue
  return xs

-- | /O(1)/ - Extract number of elements that is currently on the queue
lengthTBFQueue :: TBFQueue a -> STM Int
lengthTBFQueue (TBFQueue bQueueTVar) = lengthBQueue <$> readTVar bQueueTVar
