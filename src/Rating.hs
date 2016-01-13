module Rating
       (
         Rating(..)
       , Info
       , Lock
       , mapRating
       , emptyRating
       , emptyInfo
       , newIndex
       , getNpcIndex
       , setNpcIndex
       ) where

import Data.List (intercalate)

data Rating = Rating {dai    :: Info, kaour :: Info
                     ,eirlys :: Info, elsie :: Info}

type Info = (Maybe Int, Lock, Stock)
type Lock = Bool
type Stock = [Int]

instance Show Rating where
  show = prettyRating

prettyRating :: Rating -> String
prettyRating r = intercalate "\n" $ map tos l
  where l = [("ダイ", dai r),     ("アイリース", eirlys r)
            ,("カオル", kaour r), ("エルシィ", elsie r)]
        tos (name, i) = "【" ++ name ++ "】\t\t=>\t" ++ case i of
          (Just n, _, _)      -> show n
          (Nothing, _, _)     -> "不明"

getNpcIndex :: String -> Rating -> Maybe Info
getNpcIndex "dai"    r = Just $ dai r
getNpcIndex "eirlys" r = Just $ eirlys r
getNpcIndex "kaour"  r = Just $ kaour r
getNpcIndex "elsie"  r = Just $ elsie r
getNpcIndex _ _ = Nothing

setNpcIndex :: String -> Info -> Rating -> Rating
setNpcIndex "dai"    i r = r {dai    = i}
setNpcIndex "eirlys" i r = r {eirlys = i}
setNpcIndex "kaour"  i r = r {kaour  = i}
setNpcIndex "elsie"  i r = r {elsie  = i}
setNpcIndex _ _ _ = error "Rating.setNpcIndex: called with unknown field."

mapRating :: (Info -> Info) -> Rating -> Rating
mapRating f Rating {dai = d, eirlys = a, kaour = k, elsie = e} =
  Rating {dai = f d, eirlys = f a, kaour = f k, elsie = f e}

emptyInfo :: Info
emptyInfo = (Nothing, False, [])

emptyRating :: Rating
emptyRating = Rating {dai    = emptyInfo, kaour = emptyInfo
                     ,eirlys = emptyInfo, elsie = emptyInfo}

newIndex :: Int -> Info
newIndex n = (Just n, False, [])
