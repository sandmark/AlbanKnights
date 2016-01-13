module Main where

import REPL.REPL (repl)
import Rating (emptyRating)
import Paths_alban_knights (version)
import Data.Version (showVersion)

main :: IO ()
main = welcome >> repl emptyRating [1..]

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
