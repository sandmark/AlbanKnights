module REPL.Stock
       (
         stock
       ) where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Text.Printf

import AlbanKnights (toKeyword, getKeywordIndex)
import REPL.NPC (npcLookup)
import Rating

stock :: [String] -> Rating -> Either String Rating
stock [] r = Left $ showStock "all" r
stock [name] r = Left $ showStock (npcLookup name) r
stock (name:keys) r
  | Nothing `elem` indices = Left "不正なキーワードが含まれています"
  | otherwise = case npcLookup name of
      "dai"    -> Right r{dai    = updateStock indices (dai r)}
      "eirlys" -> Right r{eirlys = updateStock indices (eirlys r)}
      "kaour"  -> Right r{kaour  = updateStock indices (kaour r)}
      "elsie"  -> Right r{elsie  = updateStock indices (elsie r)}
      _        -> Left "そのようなNPCは存在しません"
  where indices = map getKeywordIndex keys

updateStock :: [Maybe Int] -> Info -> Info
updateStock indices (x,y,stocks) = (x,y,stocks ++ indices')
  where indices' = map fromJust indices

showStock :: String -> Rating -> String
showStock "dai" r    = toLine ("ダイ", dai r)
showStock "kaour" r  = toLine ("カオル", kaour r)
showStock "eirlys" r = toLine ("アイリース", eirlys r)
showStock "elsie" r  = toLine ("エルシィ", elsie r)
showStock "all" r    = intercalate "\n" $ map toLine npcs
  where npcs = [("ダイ", dai r), ("アイリース", eirlys r)
               ,("カオル", kaour r), ("エルシィ", elsie r)]
showStock _ _ = "そのようなNPCは存在しません"

toLine :: (String, Info) -> String
toLine (name, (_,_,[])) = printf "【%s】\t無し" name
toLine (name, (_,_,stocks)) = printf "【%s】\t%s" name keywords
  where keywords = intercalate "," $ map toKeyword stocks
