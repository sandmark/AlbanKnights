module REPL.Unset (unset) where

import Rating
import REPL.NPC (npcLookup)

unset :: [String] -> Rating -> Either String Rating
unset [] _ = Left "使い方: 'unset NPC名'"
unset (name:_) r = case npcLookup name of
  "dai"    -> Right $ r {dai = emptyIndex}
  "kaour"  -> Right $ r {kaour = emptyIndex}
  "elsie"  -> Right $ r {elsie = emptyIndex}
  "eirlys" -> Right $ r {eirlys = emptyIndex}
  s        -> Left $ "'" ++ s ++ "' could not be found."
