module Main where

import qualified AlbanKnights as AK
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Control.Monad (when)

data Rating = Rating {dai    :: Maybe Int, kaour :: Maybe Int
                     ,eirlys :: Maybe Int, elsie :: Maybe Int}

prettyRating :: Rating -> String
prettyRating r = List.intercalate "\n" $ map tos l
  where l = [("ダイ", dai r),     ("アイリース", eirlys r)
            ,("カオル", kaour r), ("エルシィ", elsie r)]
        tos (name, n) = "【" ++ name ++ "】\t\t=>\t" ++ case n of
          Just n' -> show n'
          Nothing -> "不明"

instance Show Rating where
  show = prettyRating

main :: IO ()
main = repl emptyRating [1..]

emptyRating :: Rating
emptyRating = Rating {dai    = Nothing, kaour = Nothing
                     ,eirlys = Nothing, elsie = Nothing}

repl :: Rating -> [Int]-> IO ()
repl _ [] = error "empty list given."
repl r (l:ls) = do
  putStr $ "AlbanKnights(" ++ show l ++ "): "
  hFlush stdout
  input <- getLine
  when (isExit input) $ do
    putExitMessage r
    exitSuccess
  case words $ map Char.toLower input of
    []             -> repl r ls
    (command:args) -> case dispatch command args r of
      Left str -> do putStrLn str
                     repl r ls
      Right r' -> repl r' ls

dispatch :: String -> [String] -> Rating -> Either String Rating
dispatch cmd args r
  | isShow cmd = Left $ show r
  | isSet cmd = set args r
  | otherwise = Left $ "unknown command: '" ++ cmd ++ "'"

cmdsSet :: [String]
cmdsSet = ["set", "s"]
isSet :: String -> Bool
isSet = isCmd cmdsSet

set :: [String] -> Rating -> Either String Rating
set [] _ = Left "'set' called with no arguments."
set [_] _ = Left "'set' called with invalid number of arguments."
set (npc:pos:_) r = case fromMaybe npc (lookup npc names) of
  "dai"    -> Right $ r {dai = n}
  "kaour"  -> Right $ r {kaour = n}
  "elsie"  -> Right $ r {elsie = n}
  "eirlys" -> Right $ r {eirlys = n}
  s     -> Left $ "'" ++ s ++ "' could not be found."
  where n     = Just (string2int pos)
        names = [("d","dai")
                ,("kaoru","kaour")
                ,("k","kaour")
                ,("a","eirlys")
                ,("e","elsie")
                ,("airi-su","eirlys")
                ,("erusii","elsie")
                ,("dai","dai")
                ,("kaour","kaour")
                ,("elsie","elsie")
                ,("eirlys","eirlys")
                ]

cmdsShow :: [String]
cmdsShow = ["ls","show","list"]
isShow :: String -> Bool
isShow = isCmd cmdsShow

cmdsExit :: [String]
cmdsExit = ["q", "exit", "quit", ":q"]
isExit :: String -> Bool
isExit = isCmd cmdsExit

isCmd :: [String] -> String -> Bool
isCmd = flip elem

putExitMessage :: Rating -> IO ()
putExitMessage r = do
  putStrLn "\nここでセットされた番号は保存されません"
  putStrLn "どこかにメモしておくことを忘れずに\n"
  putStrLn "--------------------------------------------------"
  print r
  putStrLn "--------------------------------------------------\n"
  putStrLn "=== Enter キーで終了します ==="
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
