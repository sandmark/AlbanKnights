module REPL.Exit
       (
         putExitMessage
       ) where

import Rating (Rating)

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
