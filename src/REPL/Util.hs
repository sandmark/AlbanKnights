module REPL.Util
       (
         wrappedPick
       , string2int
       ) where

import Data.List (intercalate)
import qualified AlbanKnights as AK
import Rating

wrappedPick :: String -> String -> Int -> Lock -> String
wrappedPick name s i l = toString name $ AK.unsafePick s (i-1)
  where toString name' keys =
          let locked = if l then " [固定]" else "\t"
          in "【" ++ name' ++ "】\t" ++ show i ++ locked ++ "\t" ++
             intercalate " -> " keys

string2int :: String -> Int
string2int str = case reads str :: [(Int, String)] of
  [(i,_)] -> if i < 1 then 1 else i
  []      -> 1
