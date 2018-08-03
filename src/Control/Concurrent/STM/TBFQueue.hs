-- |
-- Module      : Control.Concurrent.STM.TBFQueue
-- Copyright   : (c) FP Complete 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Transactional Bouded Flush Queue is a very similar to `Control.Concurrent.BFQueue.BFQueue`, with
-- an exception that it runs in `STM` and is also less efficient, but is still much faster than
-- `Control.Concurrent.STM.TBQueue.TBQueue`.
--
module Control.Concurrent.STM.TBFQueue
  ( TBFQueue
  , newTBFQueue
  , newTBFQueueIO
  , writeTBFQueue
  , tryWriteTBFQueue
  , flushTBFQueue
  , lengthTBFQueue
  ) where

import Control.Concurrent.BList
import Control.Concurrent.STM


-- | Bounded Flush Queue. It's a queue that allows pushing elements onto, but popping elements is
-- not an option. Only flushing the queue will make space for new elements.
newtype TBFQueue a = TBFQueue (TVar (BList a))

-- | Construct a new empty Flush Bounded Queue
newTBFQueue :: Int -- ^ Maximum number of elements, that this queue can hold.
           -> STM (TBFQueue a)
newTBFQueue bound = TBFQueue <$> newTVar (newBList bound)


-- | Construct a new empty Flush Bounded Queue inside IO monad.
newTBFQueueIO :: Int -- ^ Maximum number of elements, that this queue can hold.
              -> IO (TBFQueue a)
newTBFQueueIO bound = TBFQueue <$> newTVarIO (newBList bound)

-- | /O(1)/ - Push an element onto the queue. Will block if maximum bound has been reached.
writeTBFQueue :: TBFQueue a -> a -> STM ()
writeTBFQueue (TBFQueue bListTVar) x = do
  bList <- readTVar bListTVar
  case consBList x bList of
    Just newList -> writeTVar bListTVar newList
    Nothing -> retry

-- | /O(1)/ - Try to push an element onto the queue without blocking. Will return `True` if element
-- was pushed successfully, and `False` in case when the queue is full.
tryWriteTBFQueue :: TBFQueue a -> a -> STM Bool
tryWriteTBFQueue (TBFQueue bListTVar) x = do
  bList <- readTVar bListTVar
  case consBList x bList of
    Just newList -> writeTVar bListTVar newList >> return True
    Nothing      -> return False

-- | /O(n)/ - Flush the queue, unblock all the possible writers and return all the elements from the
-- queue in FIFO order.
flushTBFQueue :: TBFQueue a -> STM [a]
flushTBFQueue (TBFQueue bListTVar) = do
  bList <- readTVar bListTVar
  let (xs, newList) = flushBList bList
  writeTVar bListTVar newList
  return $! reverse xs


-- | /O(1)/ - Extract number of elements that is currently on the queue
lengthTBFQueue :: TBFQueue a -> STM Int
lengthTBFQueue (TBFQueue bListTVar) = lengthBList <$> readTVar bListTVar
