{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module PQueue (PQueue (Empty, (:<|)), empty, null, singleton, view, insert) where

import Data.Function (on)
import qualified Data.IntMap as M
import Data.List (intercalate)
import Prelude hiding (null)

newtype PQueue a = PQ {entries :: M.IntMap [a]}

{-# COMPLETE Empty, (:<|) #-} -- this defines the pattern synonyms as complete, so that WIncomplete can be used in the type signature

pattern Empty :: PQueue a
pattern Empty <-
  (null -> True)
  where
    Empty = PQ M.empty

pattern (:<|) :: a -> PQueue a -> PQueue a
pattern h :<| q <- (view -> Just (h, q))

instance Show a => Show (PQueue a) where
  show (PQ m) = "fromList " ++ intercalate ", " [show (k, v) | (k, vs) <- M.toList m, v <- vs]

empty :: PQueue a
empty = PQ M.empty

null :: PQueue a -> Bool
null (PQ s) = M.null s

singleton :: Int -> a -> PQueue a
singleton p a = PQ $ M.singleton p [a]

insert :: Int -> a -> PQueue a -> PQueue a
insert p e (PQ q) = PQ $ M.insertWith (++) p [e] q

view :: PQueue a -> Maybe (a, PQueue a)
view pq = viewWithPrio pq >>= \(_, e, q) -> return (e, q)

-- extract the first element from the queue
viewWithPrio :: PQueue a -> Maybe (Int, a, PQueue a)
viewWithPrio (PQ q) =
  M.minViewWithKey q >>= \((p, xs), q') ->
    case xs of
      [] -> error "PQueue.view: empty list"
      [x] -> Just (p, x, PQ q')
      x : xs' -> Just (p, x, PQ $ M.insert p xs' q)