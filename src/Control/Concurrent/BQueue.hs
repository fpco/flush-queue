{-# LANGUAGE NamedFieldPuns #-}
module Control.Concurrent.BQueue
  ( BQueue
  , newBQueue
  , pushBQueue
  , popBQueue
  , takeBQueue
  , flushBQueue
  , lengthBQueue
  ) where

import Data.Foldable as F
import Data.Sequence as Seq

-- | FIFO Bounded Queue
data BQueue a = BQueue
  { bqSeq      :: !(Seq a)
  , bqMaxCount :: {-# UNPACK #-} !Int
  }

-- | Create new Bounded Queue
newBQueue :: Int -- ^ Upper bound on the numer of elements the queue can hold.
          -> BQueue a
newBQueue bqMaxCount = BQueue {bqSeq = mempty, bqMaxCount}

-- | Push an element onto the queue. Returns the new queue with the element placed onto the right
-- side of the source queue, but only if the maximum bound hasn't been reached, otherwise it will
-- return `Nothing`
pushBQueue :: a -> BQueue a -> Maybe (BQueue a)
pushBQueue x bq@BQueue {bqSeq}
  | Seq.length bqSeq < bqMaxCount bq = Just bq {bqSeq = bqSeq :|> x}
  | otherwise = Nothing


-- | Pop an element from the queue. Returns the leftmost element from the queue together with new
-- queue, lacking that element, `Nothing` if the queue was empty.
popBQueue :: BQueue a -> Maybe (a, BQueue a)
popBQueue bq =
  case bqSeq bq of
    Empty -> Nothing
    (x :<| newSeq) -> Just (x, bq {bqSeq = newSeq})



-- | /O(n) - Get all the elements from the Bounded Queue.
flushBQueue :: BQueue a -> ([a], BQueue a)
flushBQueue bq@BQueue {bqSeq} = (F.toList bqSeq, bq {bqSeq = mempty})

-- | /O(i)/ - Take @i@ elements from the Bounded Queue. This function doesn't fail - it returns empty
-- list on negative @i@ and all elements there is if requested more than available.
takeBQueue :: Int -> BQueue a -> ([a], BQueue a)
takeBQueue i bq@BQueue {bqSeq} = (F.toList leftSeq, bq {bqSeq = rightSeq})
  where (leftSeq, rightSeq) = Seq.splitAt i bqSeq


-- | /O(1)/ - Get the current length of a queue
lengthBQueue :: BQueue a -> Int
lengthBQueue BQueue {bqSeq} = Seq.length bqSeq
