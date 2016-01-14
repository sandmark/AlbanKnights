module REPL.Lock
       (
         lock
       ) where

import Data.Maybe (fromMaybe)
import Rating
import REPL.Commands (npcNames)

lock :: [String] -> Rating -> Either String Rating
lock [] _ = Left "NPCを指定してください"
lock (arg:_) r =
  case getNpcIndex name r of
    Nothing -> Left "そのようなNPCは存在しません"
    Just (i, locked, st) -> Right $ setNpcIndex name (i, not locked, st) r
  where name = fromMaybe arg (lookup arg npcNames)
