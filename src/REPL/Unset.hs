module REPL.Unset (unset) where

import Rating
import REPL.NPC (npcLookup)

unset :: [String] -> Rating -> Either String Rating
unset [] _ = Left "使い方: 'unset NPC名'"
unset (name:_) r = case npcLookup name of
  "dai"    -> Right $ r {dai = emptyInfo}
  "kaour"  -> Right $ r {kaour = emptyInfo}
  "elsie"  -> Right $ r {elsie = emptyInfo}
  "eirlys" -> Right $ r {eirlys = emptyInfo}
  s        -> Left $ "'" ++ s ++ "' could not be found."
