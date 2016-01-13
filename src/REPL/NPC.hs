module REPL.NPC
       (
         npc
       , npcLookup
       ) where

import Data.Maybe (fromMaybe)

import Rating
import REPL.Commands (npcNames)
import REPL.Util

npc :: String -> [String] -> Rating -> Either String Rating
npc name args r = case npcLookup name of
  "dai" -> Left $ npcKeywords "dai" "ダイ" (dai r) args
  "kaour" -> Left $ npcKeywords "kaour" "カオル" (kaour r) args
  "elsie" -> Left $ npcKeywords "elsie" "エルシィ" (elsie r) args
  "eirlys" -> Left $ npcKeywords "eirlys" "アイリース" (eirlys r) args
  _     -> Left "そのようなNPCは存在しません"

npcKeywords :: String -> String -> Index -> [String] -> String
npcKeywords key name index args = case args of
  (i:_) -> wrappedPick name key (string2int i) False
  _     -> case index of
    (Just i, l)  -> wrappedPick name key i l
    (Nothing, _) -> name ++ "の番号は未設定です。\n" ++
                    "'" ++ key ++ " 1' のようにして指定するか、" ++
                    "'set' コマンドを使ってください。"

npcLookup name = fromMaybe name (lookup name npcNames)
