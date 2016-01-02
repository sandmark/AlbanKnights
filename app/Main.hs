module Main where

import qualified AlbanKnights as AK
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

main :: IO ()
main = mapM_ repl [1..]

repl :: Int -> IO ()
repl l = do
  putStr $ "AlbanKnights(" ++ show l ++ "): "
  hFlush stdout
  input <- getLine
  case words $ map Char.toLower input of
    (command:args) -> dispatch command args
    []             -> return ()

dispatch :: String -> [String] -> IO ()
dispatch "exit" _ = putExitMessage >> exitSuccess
dispatch command _ = putStrLn $ "unknown command: '" ++ command ++ "'"

putExitMessage :: IO ()
putExitMessage = do
  putStrLn "\nここでセットされた番号は保存されません"
  putStrLn "どこかにメモしておくことを忘れずに\n"
  putStrLn "--- Enter キーで終了します ---"
  _ <- getLine
  return ()

string2int :: String -> Int
string2int str = read str :: Int

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
