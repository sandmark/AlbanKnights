module REPL.Suggest (suggest) where

import Text.Printf
import Data.List (intercalate)
import Data.Maybe (fromJust)

import REPL.NPC (npcLookup)
import Rating
import AlbanKnights (findKeyIndices, pickOne, getKeywordIndex)

suggest :: [String] -> Rating -> Either String Rating
suggest [str] r = case npcLookup str of
  "dai"    -> Left $ toLine "dai"    "ダイ"       (dai r)
  "kaour"  -> Left $ toLine "kaour"  "カオル"     (kaour r)
  "eirlys" -> Left $ toLine "eirlys" "アイリース" (eirlys r)
  "elsie"  -> Left $ toLine "elsie"  "エルシィ"   (elsie r)
  _        -> Left "そのようなNPCは存在しません"

suggest [] r = Left $ intercalate "\n" $ map wrapToLine table
  where table = [("dai",    "ダイ",       dai r)
                ,("eirlys", "アイリース", eirlys r)
                ,("kaour",  "カオル",     kaour r)
                ,("elsie",  "エルシィ",   elsie r)]
        wrapToLine (npc, name, info) = toLine npc name info

suggest (str:keywords) _
  | Nothing `elem` indices = Left "不正なキーワードが含まれています"
  | otherwise              = case npcLookup str of
      "dai"    -> Left $ toLine "dai"    "ダイ"       (Nothing, False, toStock indices)
      "kaour"  -> Left $ toLine "kaour"  "カオル"     (Nothing, False, toStock indices)
      "eirlys" -> Left $ toLine "eirlys" "アイリース" (Nothing, False, toStock indices)
      "elsie"  -> Left $ toLine "elsie"  "エルシィ"   (Nothing, False, toStock indices)
      _        -> Left "そのようなNPCは存在しません"
  where indices = map getKeywordIndex keywords
        toStock = map fromJust

toLine :: String -> String-> Info -> String
toLine _ name (_,_,[]) = printf "【%s】\tストックが空です" name
toLine npc name (_,_,keys) = case findKeyIndices npc keys of
  [] -> printf "【%s】\t該当キーワードがありません" name
  xs -> printf "【%s】\t" name ++ intercalate "/" (map suggestion xs)
  where suggestion i = printf "%s[%d]" (pickOne npc i) i
