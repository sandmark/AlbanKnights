module REPL.Commands
       (
         isCmd
       , isExit
       , isList
       , isUpdate
       , isSet
       , isNPC
       , isUnset
       , isLock
       , isStock
       , npcNames
       ) where

-- --------------------
-- Command/Alias List
--
cmdsStock :: [String]
cmdsStock = ["stock", "st"]

cmdsUpdate :: [String]
cmdsUpdate = ["update","next","x","up"]

cmdsExit :: [String]
cmdsExit = ["q", "exit", "quit", ":q"]

cmdsList :: [String]
cmdsList = ["ls","show","list","l"]

cmdsSet :: [String]
cmdsSet = ["set", "s"]

cmdsUnset :: [String]
cmdsUnset = ["unset","u"]

cmdsLock :: [String]
cmdsLock = ["lock","lo","const","c","bind","b","unlock","unbind","toggle"
           ,"hold","unhold","h"]

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

-- --------------------
-- Export Functions
--

isStock :: String -> Bool
isStock = isCmd cmdsStock

isLock :: String -> Bool
isLock = isCmd cmdsLock

isUpdate :: String -> Bool
isUpdate = isCmd cmdsUpdate

isNPC :: String -> Bool
isNPC name = case lookup name npcNames of
  Just _  -> True
  Nothing -> False

isUnset :: String -> Bool
isUnset = isCmd cmdsUnset

isSet :: String -> Bool
isSet = isCmd cmdsSet

isList :: String -> Bool
isList = isCmd cmdsList

isExit :: String -> Bool
isExit = isCmd cmdsExit

isCmd :: [String] -> String -> Bool
isCmd = flip elem
