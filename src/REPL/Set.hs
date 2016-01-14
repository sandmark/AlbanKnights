module REPL.Set (set) where

import Rating
import REPL.NPC (npcLookup)
import REPL.Util

set :: [String] -> Rating -> Either String Rating
set [] _ = Left "使い方: 'set NPC名 番号'"
set [_] _ = Left "使い方: 'set NPC名 番号'"
set (name:pos:_) r = case npcLookup name of
  "dai"    -> Right $ r {dai = newIndex n}
  "kaour"  -> Right $ r {kaour = newIndex n}
  "elsie"  -> Right $ r {elsie = newIndex n}
  "eirlys" -> Right $ r {eirlys = newIndex n}
  s        -> Left $ "'" ++ s ++ "' could not be found."
  where n     = string2int pos
