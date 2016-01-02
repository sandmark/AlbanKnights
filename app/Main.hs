module Main where

import qualified AlbanKnights as AK
import System.Environment (getArgs, getProgName)
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)

key2npc :: String -> String
key2npc key = fromMaybe key (Map.lookup key names)
  where names =
          Map.fromList [("dai",   "ダイ"),   ("eirlys", "アイリース")
                       ,("kaour", "カオル"), ("elsie",  "エルシィ")]

npc2key :: String -> String
npc2key str = map Char.toLower $ fromMaybe str (Map.lookup str table)
  where table =
          Map.fromList
          [("ダイ", "dai")
          ,("アイリース", "eirlys")
          ,("airi-su", "eirlys")
          ,("airiisu", "eirlys")
          ,("カオル", "kaour")
          ,("kaoru", "kaour")
          ,("erusii", "elsie")
          ,("erusixi", "elsie")
          ,("erusili", "elsie")
          ]

string2int :: String -> Int
string2int str = read str :: Int

putUsage :: IO ()
putUsage = do
  progname <- getProgName
  putStrLn $ "使い方: " ++ progname ++ " NPC 番号\n"
  putStrLn "NPCには以下のように入力してください"
  putStrLn $ "Dai   <- ダイ" ++ '\t':'\t':"Eirlys <- アイリース"
  putStrLn $ "Kaour <- カオル" ++ '\t':'\t':"Elsie  <- エルシィ\n"
  putStrLn $ "例: " ++ progname ++ " Kaour 1\n"
  putStrLn "大文字小文字は区別されません"
  putStrLn "なんとなくローマ字でも大丈夫です\n"
  putStrLn $ "例: " ++ progname ++ " airi-su 34"
  putStrLn $ "例: " ++ progname ++ " erusii 55"

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then putUsage
    else let name = npc2key $ head args
             pos  = string2int (head $ tail args) - 1
         in case AK.pick name pos of
    Left _ -> putStrLn $ "`" ++ name ++ "' という名前のNPCが見つかりません"
    Right keywords ->
      let result = "【" ++ key2npc name ++ "】  " ++
            List.intercalate " -> " keywords
          msg    = "次の番号は " ++ show next ++ " です"
      in putStrLn $ List.intercalate "\n" [result, msg]
      where next = let i = pos + 3 + 1
                   in if i > 97 then i - 97 else i
