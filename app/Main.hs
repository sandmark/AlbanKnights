module Main where

import qualified AlbanKnights as AK
import Rating
import Data.List (intercalate)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Paths_alban_knights (version)
import Data.Version (showVersion)

main :: IO ()
main = welcome >> repl emptyRating [1..]

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
  | isUnset cmd = unset args r
  | isUpdate cmd = update r
  | isLock cmd = lock args r
  | isNPC cmd = npc cmd args r
  | otherwise = Left $ "unknown command: '" ++ cmd ++ "'"

cmdsLock :: [String]
cmdsLock = ["lock","lo","const","c","bind","b","unlock","unbind","toggle"
           ,"hold","unhold","h"]

isLock :: String -> Bool
isLock = isCmd cmdsLock

lock :: [String] -> Rating -> Either String Rating
lock [] _ = Left "NPCを指定してください"
lock (arg:_) r =
  case getNpcIndex name r of
    Nothing -> Left "そのようなNPCは存在しません"
    Just (i, locked) -> Right $ setNpcIndex name (i, not locked) r
  where name = fromMaybe arg (lookup arg npcNames)

cmdsUpdate :: [String]
cmdsUpdate = ["update","next","x","up"]

isUpdate :: String -> Bool
isUpdate = isCmd cmdsUpdate

update :: Rating -> Either String Rating
update r = Right $ mapRating updateEach r
  where updateEach (Nothing, l) = (Nothing, l)
        updateEach (Just x, True) = (Just x, True)
        updateEach (Just x, False) = (Just $ up x, False)
          where up n = let n' = n + 3
                       in if n' > 97 then n' - 97 else n'

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

npcKeywords :: String -> String -> Index -> [String] -> String
npcKeywords key name index args = case args of
  (i:_) -> wrappedPick name key (string2int i) False
  _     -> case index of
    (Just i, l)  -> wrappedPick name key i l
    (Nothing, _) -> name ++ "の番号は未設定です。\n" ++
                    "'" ++ key ++ " 1' のようにして指定するか、" ++
                    "'set' コマンドを使ってください。"

wrappedPick :: String -> String -> Int -> Lock -> String
wrappedPick name s i l = toString name $ AK.unsafePick s (i-1)
  where toString name' keys =
          let locked = if l then " [固定]" else "\t"
          in "【" ++ name' ++ "】\t" ++ show i ++ locked ++ "\t" ++
             intercalate " -> " keys

cmdsUnset :: [String]
cmdsUnset = ["unset","u"]
isUnset :: String -> Bool
isUnset = isCmd cmdsUnset

unset :: [String] -> Rating -> Either String Rating
unset [] _ = Left "使い方: 'unset NPC名'"
unset (name:_) r = case fromMaybe name (lookup name npcNames) of
  "dai"    -> Right $ r {dai = emptyIndex}
  "kaour"  -> Right $ r {kaour = emptyIndex}
  "elsie"  -> Right $ r {elsie = emptyIndex}
  "eirlys" -> Right $ r {eirlys = emptyIndex}
  s        -> Left $ "'" ++ s ++ "' could not be found."

cmdsSet :: [String]
cmdsSet = ["set", "s"]
isSet :: String -> Bool
isSet = isCmd cmdsSet

set :: [String] -> Rating -> Either String Rating
set [] _ = Left "使い方: 'set NPC名 番号'"
set [_] _ = Left "使い方: 'set NPC名 番号'"
set (name:pos:_) r = case fromMaybe name (lookup name npcNames) of
  "dai"    -> Right $ r {dai = newIndex n}
  "kaour"  -> Right $ r {kaour = newIndex n}
  "elsie"  -> Right $ r {elsie = newIndex n}
  "eirlys" -> Right $ r {eirlys = newIndex n}
  s        -> Left $ "'" ++ s ++ "' could not be found."
  where n     = string2int pos

cmdsList :: [String]
cmdsList = ["ls","show","list","l"]
isList :: String -> Bool
isList = isCmd cmdsList

list :: Rating -> Either String Rating
list r = Left $ intercalate "\n" $ map f npcs
  where npcs = [("ダイ","dai",dai r),("アイリース","eirlys",eirlys r)
               ,("カオル","kaour",kaour r),("エルシィ","elsie",elsie r)
               ]
        f (name,key,(i,l)) = case i of
          Just n  -> wrappedPick name key n l
          Nothing -> "【" ++ name ++ "】\t(不明)"
{-
【ダイ】        [1]   => 遊び(1)          -> 恋愛(2)  -> 遊び(3)
【アイリース】 [96]   => 恋愛(96)         -> 料理(97) -> 任務(1)
【カオル】     [不明]
【エルシィ】   [77]   => ファッション(77) -> 恋愛(78) -> 任務(79)
-}

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
string2int str = case reads str :: [(Int, String)] of
  [(i,_)] -> i
  []      -> 1

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

logo :: String
logo =
  "    _   _ _                 _  __     _      _   _      \n" ++
  "   /_\\ | | |__  __ _ _ _   | |/ /_ _ (_)__ _| |_| |_ ___\n" ++
  "  / _ \\| | '_ \\/ _` | ' \\  | ' <| ' \\| / _` | ' \\  _(_-<\n" ++
  " /_/ \\_\\_|_.__/\\__,_|_||_| |_|\\_\\_||_|_\\__, |_||_\\__/__/\n" ++
  "                                       |___/            "

welcome :: IO ()
welcome = do
  putStrLn $ logo ++ "\n"
  putStrLn $
    "Welcome to AlbanKnights " ++ showVersion version ++
    " <https://github.com/sandmark/AlbanKnights/>\n"
