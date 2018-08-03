{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Control.Concurrent.BFQueue
-- Copyright   : (c) FP Complete 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@fpcomplete.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Bouded Flush Queue is a very efficient queue that supports pushing elements concurrently, but has
-- no support for popping elements from the queue. The only way to get elements from the queue is to
-- flush it and get all the elements in FIFO ourder.
--
module Control.Concurrent.BFQueue
  ( BFQueue
  , newBFQueue
  , writeBFQueue
  , tryWriteBFQueue
  , flushBFQueue
  , lengthBFQueue
  ) where

import Data.Atomics (atomicModifyIORefCAS)
import Data.IORef
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.BList

-- | Bounded Flush Queue. It's a queue that allows pushing elements onto, but popping elements is
-- not an option. Only flushing the queue will make space for new elements.
newtype BFQueue a = BFQueue (IORef (BList a, MVar ()))

-- | Create new empty BFQueue
newBFQueue :: Int -> IO (BFQueue a)
newBFQueue bound = do
  baton <- newEmptyMVar
  bListIORef <- newIORef (newBList bound, baton)
  return $ BFQueue bListIORef

-- | /O(1)/ - Push an element onto the queue. Will block if maximum bound has been reached.
writeBFQueue :: BFQueue a -> a -> IO ()
writeBFQueue (BFQueue bListIORef) x = inner
  where
    inner = join $ atomicModifyIORefCAS bListIORef $ \bbList@(bList, baton) ->
      case consBList x bList of
        Just newList -> ((newList, baton), pure ())
        Nothing -> (bbList, readMVar baton >> inner)

-- | /O(1)/ - Try to push an element onto the queue without blocking. Will return `True` if element
-- was pushed successfully, and `False` in case when the queue is full.
tryWriteBFQueue :: BFQueue a -> a -> IO Bool
tryWriteBFQueue (BFQueue bListIORef) x =
  atomicModifyIORefCAS bListIORef $ \bbList@(bList, baton) ->
    case consBList x bList of
      Just newList -> ((newList, baton), True)
      Nothing -> (bbList, False)


-- | /O(n)/ - Flush the queue, unblock all the possible writers and return all the elements from the
-- queue in FIFO order.
flushBFQueue :: BFQueue a -> IO [a]
flushBFQueue (BFQueue bListIORef) = do
  newBaton <- newEmptyMVar
  join $
    atomicModifyIORefCAS bListIORef $ \(bList, baton) ->
      let !(xs, newList) = flushBList bList
          queue = reverse xs
       in queue `seq` ((newList, newBaton), queue <$ putMVar baton ())


-- | /O(1)/ - Extract number of elements that is currently on the queue
lengthBFQueue :: BFQueue a -> IO Int
lengthBFQueue (BFQueue bListIORef) = lengthBList . fst <$> readIORef bListIORef
