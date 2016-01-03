module Main where

import qualified AlbanKnights as AK
import Data.List (intercalate)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Control.Monad (when)

data Rating = Rating {dai    :: Maybe Int, kaour :: Maybe Int
                     ,eirlys :: Maybe Int, elsie :: Maybe Int}

prettyRating :: Rating -> String
prettyRating r = intercalate "\n" $ map tos l
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
  case words $ map toLower input of
    []             -> repl r ls
    (command:args) -> case dispatch command args r of
      Left str -> do putStrLn str
                     repl r ls
      Right r' -> repl r' ls

dispatch :: String -> [String] -> Rating -> Either String Rating
dispatch cmd args r
  | isList cmd = list r
  | isSet cmd = set args r
  | isNPC cmd = npc cmd args r
  | otherwise = Left $ "unknown command: '" ++ cmd ++ "'"

isNPC :: String -> Bool
isNPC name = case lookup name npcNames of
  Just _  -> True
  Nothing -> False

npc :: String -> [String] -> Rating -> Either String Rating
npc name args r = case fromMaybe name (lookup name npcNames) of
  "dai" -> Left $ npcKeywords "dai" "ダイ" (dai r) args
  "kaour" -> Left $ npcKeywords "kaour" "カオル" (kaour r) args
  "elsie" -> Left $ npcKeywords "elsie" "エルシィ" (elsie r) args
  "eirlys" -> Left $ npcKeywords "eirlys" "アイリース" (eirlys r) args
  _     -> Left "そのようなNPCは存在しません"

npcKeywords :: String -> String -> Maybe Int -> [String] -> String
npcKeywords key name rate args = case rate of
  Just i -> right name $ wrappedPick key i
  Nothing -> case args of
    (i:_) -> right name $ wrappedPick key (string2int i)
    _     -> left name
  where wrappedPick s i = fromRight $ AK.pick s (i-1)
        right name' keys = "【" ++ name' ++ "】  " ++ intercalate " -> " keys
        left  name' = name' ++ "の番号は未設定です。\n" ++
                      "'" ++ key ++ " 1' のようにして指定するか、" ++
                      "'set' コマンドを使ってください。"

cmdsSet :: [String]
cmdsSet = ["set", "s"]
isSet :: String -> Bool
isSet = isCmd cmdsSet

set :: [String] -> Rating -> Either String Rating
set [] _ = Left "使い方: 'set NPC名 番号'"
set [_] _ = Left "使い方: 'set NPC名 番号'"
set (name:pos:_) r = case fromMaybe name (lookup name npcNames) of
  "dai"    -> Right $ r {dai = n}
  "kaour"  -> Right $ r {kaour = n}
  "elsie"  -> Right $ r {elsie = n}
  "eirlys" -> Right $ r {eirlys = n}
  s     -> Left $ "'" ++ s ++ "' could not be found."
  where n     = Just (string2int pos)

cmdsList :: [String]
cmdsList = ["ls","show","list"]
isList :: String -> Bool
isList = isCmd cmdsList

list :: Rating -> Either String Rating
list = undefined

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

npcNames :: [(String, String)]
npcNames = [("d","dai")
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
           ,("ダイ", "dai")
           ,("カオル", "kaour")
           ,("アイリース", "eirlys")
           ,("エルシィ", "elsie")
           ,("erusixi", "elsie")
           ,("erusili", "elsie")
           ]

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "fromRight: Argument takes from 'Left _'"
