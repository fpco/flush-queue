{-# LANGUAGE NamedFieldPuns #-}
module Control.Concurrent.BList
  ( BList
  , newBList
  , consBList
  , flushBList
  , lengthBList
  ) where

-- | Bounded List
data BList a = BList
  { blCount    :: {-# UNPACK #-} !Int
  , blList     :: ![a]
  , blMaxCount :: {-# UNPACK #-} !Int
  }

-- | Create new Bounded List
newBList :: Int -- ^ Upper bound on the numer of elements the list can hold.
         -> BList a
newBList blMaxCount = BList {blCount = 0, blList = [], blMaxCount}

-- | Cons an element on the list. Returns the a new list with the element consed onto the top, but
-- only if the maximum bound hasn't been reached, otherwise it will return a `Nothing`
consBList :: a -> BList a -> Maybe (BList a)
consBList x bl@BList {blCount, blList, blMaxCount}
  | blCount < blMaxCount = Just bl {blCount = 1 + blCount, blList = x : blList}
  | otherwise = Nothing


-- | Pull out the list from from Bounded List, while possibly unblocking the writers that
-- are waiting for the list to get emptied.
flushBList :: BList a -> ([a], BList a)
flushBList bl@BList {blList} = (blList, bl {blCount = 0, blList = []})


-- | /O(1) - Pull out the list from from Bounded List, while possibly unblocking the writers that
-- are waiting for the list to get emptied.
lengthBList :: BList a -> Int
lengthBList BList {blCount} = blCount


