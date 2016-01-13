module AlbanKnights
    (
      pick
    , unsafePick
    , toKeyword
    ) where

toKeyword :: Int -> String
toKeyword = (keywords !!)

pick :: String -> Int -> Either String [String]
pick key i = case lookup key table of
  Just keys -> Right $ map toKeyword $ take 3 $ drop i keys
  Nothing   -> Left $ "No NPC found named " ++ key ++ "."

unsafePick :: String -> Int -> [String]
unsafePick key i = case lookup key table of
  Just keys -> map toKeyword $ take 3 $ drop i keys
  Nothing   -> error "AlbanKnights.unsafePick: called with invalid npc name."

keywords :: [String]
keywords = map head keywordAliases

keywordAliases :: [[String]]
keywordAliases = [["任務", "n", "m", "ninmu", "ninnmu", "mission"]
                 ,["訓練", "t", "k", "kunren", "training"]
                 ,["遊び", "p", "a", "play", "asobi", "playing"]
                 ,["料理", "c", "ry", "cook", "ryouri", "cooking"]
                 ,["ファッション", "f", "fasshon", "fassyon", "fashion"]
                 ,["恋愛", "l", "re", "renai", "rennai", "lo", "love"]
                 ]

table :: [(String, [Int])]
table =
  [("dai",
    cycle [2,5,2,4,1,2,2,2,2,3,2,1,2,5,0,5,4,2,2,3,4,2,2,4,4,3,4,2,3,0
          ,5,2,3,2,4,4,3,2,4,1,2,4,2,5,0,5,2,5,2,4,3,4,4,2,4,1,1,2,2,2
          ,2,2,4,1,3,4,2,4,2,4,2,1,2,0,3,2,1,2,4,2,0,5,1,5,4,0,4,5,4,0
          ,4,3,2,4,3,5,4,5,4])
  ,("eirlys",
    cycle [0,2,3,2,0,1,0,0,1,0,1,4,1,1,3,5,1,4,0,1,5,4,5,5,3,1,3,2,4
          ,4,0,0,0,1,1,1,0,3,0,0,3,2,2,1,3,2,3,3,0,3,1,0,0,1,1,5,5,0
          ,1,3,2,4,1,1,3,0,1,0,0,0,2,1,1,2,1,0,1,0,1,4,0,1,3,0,4,2,1
          ,1,3,3,0,1,3,0,0,5,3,5,1])
  ,("kaour",
    cycle [2,2,1,3,2,5,1,3,5,3,0,0,4,5,3,4,5,0,0,0,4,5,0,4,2,0,4,1,5
          ,0,0,2,5,0,4,0,1,1,1,2,1,5,4,3,1,2,4,5,2,5,3,0,2,4,3,3,3,4
          ,4,2,2,1,4,1,0,1,3,5,4,1,2,4,5,1,0,0,3,0,0,4,3,3,1,4,3,5,0
          ,4,5,3,5,4,5,1,0,3,1,5,4])
  ,("elsie",
    cycle [3,2,4,5,4,2,5,5,2,0,1,2,5,2,1,2,0,3,2,2,2,5,2,3,2,5,0,4,2
          ,2,2,5,3,5,1,5,2,3,0,1,2,2,0,5,3,5,3,2,2,5,2,1,5,2,0,2,2,2
          ,4,5,2,2,0,1,5,5,2,2,2,4,2,1,3,2,4,2,4,5,0,2,2,5,1,2,2,4,5
          ,0,0,4,4,4,4,5,2,2,0,3,0])
  ]
