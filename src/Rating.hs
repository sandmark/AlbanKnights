module Rating
       (
         Rating(..)
       , Index
       , Lock
       , mapRating
       , emptyRating
       , emptyIndex
       , newIndex
       , getNpcIndex
       , setNpcIndex
       ) where

import Data.List (intercalate)

data Rating = Rating {dai    :: Index, kaour :: Index
                     ,eirlys :: Index, elsie :: Index}

type Index = (Maybe Int, Lock)
type Lock = Bool

instance Show Rating where
  show = prettyRating

prettyRating :: Rating -> String
prettyRating r = intercalate "\n" $ map tos l
  where l = [("ダイ", dai r),     ("アイリース", eirlys r)
            ,("カオル", kaour r), ("エルシィ", elsie r)]
        tos (name, i) = "【" ++ name ++ "】\t\t=>\t" ++ case i of
          (Just n, _)      -> show n
          (Nothing, _)     -> "不明"

getNpcIndex :: String -> Rating -> Maybe Index
getNpcIndex "dai"    r = Just $ dai r
getNpcIndex "eirlys" r = Just $ eirlys r
getNpcIndex "kaour"  r = Just $ kaour r
getNpcIndex "elsie"  r = Just $ elsie r
getNpcIndex _ _ = Nothing

setNpcIndex :: String -> Index -> Rating -> Rating
setNpcIndex "dai"    i r = r {dai    = i}
setNpcIndex "eirlys" i r = r {eirlys = i}
setNpcIndex "kaour"  i r = r {kaour  = i}
setNpcIndex "elsie"  i r = r {elsie  = i}
setNpcIndex _ _ _ = error "Rating.setNpcIndex: called with unknown field."

mapRating :: (Index -> Index) -> Rating -> Rating
mapRating f (Rating {dai = d, eirlys = a, kaour = k, elsie = e}) =
  Rating {dai = f d, eirlys = f a, kaour = f k, elsie = f e}

emptyIndex :: Index
emptyIndex = (Nothing, False)

emptyRating :: Rating
emptyRating = Rating {dai    = emptyIndex, kaour = emptyIndex
                     ,eirlys = emptyIndex, elsie = emptyIndex}

newIndex :: Int -> Index
newIndex n = (Just n, False)
