module REPL.REPL
       (
         repl
       ) where

import System.IO (hFlush, stdout)
import Control.Monad (when)
import System.Exit (exitSuccess)
import Data.Char (toLower)

import Rating
import REPL.Commands
import REPL.Exit
import REPL.List
import REPL.NPC
import REPL.Set
import REPL.Unset
import REPL.Lock
import REPL.Update
import REPL.Stock
import REPL.Suggest

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
  | isStock cmd = stock args r
  | isSuggest cmd = suggest args r
  | isNPC cmd = npc cmd args r
  | otherwise = Left $ "unknown command: '" ++ cmd ++ "'"
