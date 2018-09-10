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
-- flush it and get all the elements in FIFO order.
--
module Control.Concurrent.BFQueue
  ( BFQueue
  , newBFQueue
  , writeBFQueue
  , tryWriteBFQueue
  , takeBFQueue
  , flushBFQueue
  , lengthBFQueue
  ) where

import Data.Atomics (atomicModifyIORefCAS)
import Data.IORef
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.BQueue

-- | Bounded Flush Queue. It's a queue that allows pushing elements onto, but popping elements is
-- not an option. Only flushing or non-blocking taking from the queue will make space for new
-- elements and unlbock any concurrent writers..
newtype BFQueue a = BFQueue (IORef (BQueue a, MVar ()))

-- | Create new empty BFQueue
newBFQueue :: Int -> IO (BFQueue a)
newBFQueue bound = do
  baton <- newEmptyMVar
  bQueueIORef <- newIORef (newBQueue bound, baton)
  return $ BFQueue bQueueIORef

-- | /O(1)/ - Push an element onto the queue. Will block if maximum bound has been reached.
writeBFQueue :: BFQueue a -> a -> IO ()
writeBFQueue (BFQueue bQueueIORef) x = inner
  where
    inner = join $ atomicModifyIORefCAS bQueueIORef $ \bbQueue@(bQueue, baton) ->
      case pushBQueue x bQueue of
        Just newQueue -> ((newQueue, baton), pure ())
        Nothing -> (bbQueue, readMVar baton >> inner)

-- | /O(1)/ - Try to push an element onto the queue without blocking. Will return `True` if element
-- was pushed successfully, and `False` in case when the queue is full.
tryWriteBFQueue :: BFQueue a -> a -> IO Bool
tryWriteBFQueue (BFQueue bQueueIORef) x =
  atomicModifyIORefCAS bQueueIORef $ \bbQueue@(bQueue, baton) ->
    case pushBQueue x bQueue of
      Just newQueue -> ((newQueue, baton), True)
      Nothing -> (bbQueue, False)

-- | /O(n)/ - Flush the queue, unblock all the possible writers and return all the elements from the
-- queue in FIFO order.
flushBFQueue :: BFQueue a -> IO [a]
flushBFQueue (BFQueue bQueueIORef) = do
  newBaton <- newEmptyMVar
  join $
    atomicModifyIORefCAS bQueueIORef $ \(bQueue, baton) ->
      let !(queue, newQueue) = flushBQueue bQueue
       in ((newQueue, newBaton), queue <$ putMVar baton ())

-- | /O(i)/ - Take @i@ elements from the queue, unblock all the possible writers and return all the
-- elements from the queue in FIFO order.
takeBFQueue :: Int -> BFQueue a -> IO [a]
takeBFQueue i (BFQueue bQueueIORef)
  | i <= 0 = return []
  | otherwise = do
    newBaton <- newEmptyMVar
    join $
      atomicModifyIORefCAS bQueueIORef $ \(bQueue, baton) ->
        let !(queue, newQueue) = takeBQueue i bQueue
         in ((newQueue, newBaton), queue <$ putMVar baton ())


-- | /O(1)/ - Extract number of elements that is currently on the queue
lengthBFQueue :: BFQueue a -> IO Int
lengthBFQueue (BFQueue bQueueIORef) = lengthBQueue . fst <$> readIORef bQueueIORef
