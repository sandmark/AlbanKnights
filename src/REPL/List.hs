module REPL.List (list) where

import Data.List (intercalate)
import Rating
import REPL.Util

list :: Rating -> Either String Rating
list r = Left $ intercalate "\n" $ map f npcs
  where npcs = [("ダイ","dai",dai r),("アイリース","eirlys",eirlys r)
               ,("カオル","kaour",kaour r),("エルシィ","elsie",elsie r)
               ]
        f (name,key,(i,l,_)) = case i of
          Just n  -> wrappedPick name key n l
          Nothing -> "【" ++ name ++ "】\t(不明)"
{-
【ダイ】        [1]   => 遊び(1)          -> 恋愛(2)  -> 遊び(3)
【アイリース】 [96]   => 恋愛(96)         -> 料理(97) -> 任務(1)
【カオル】     [不明]
【エルシィ】   [77]   => ファッション(77) -> 恋愛(78) -> 任務(79)
-}
