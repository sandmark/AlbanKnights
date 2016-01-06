module Rating
       (
         Rating(..)
       , mapRating
       , emptyRating
       ) where

import Data.List (intercalate)

data Rating = Rating {dai    :: Maybe Int, kaour :: Maybe Int
                     ,eirlys :: Maybe Int, elsie :: Maybe Int}

prettyRating :: Rating -> String
prettyRating r = intercalate "\n" $ map tos l
  where l = [("ダイ", dai r),     ("アイリース", eirlys r)
            ,("カオル", kaour r), ("エルシィ", elsie r)]
        tos (name, n) = "【" ++ name ++ "】\t\t=>\t" ++ case n of
          Just n' -> show n'
          Nothing -> "不明"

mapRating :: (Maybe Int -> Maybe Int) -> Rating -> Rating
mapRating f (Rating {dai = d, eirlys = a, kaour = k, elsie = e}) =
  Rating {dai = f d, eirlys = f a, kaour = f k, elsie = f e}

emptyRating :: Rating
emptyRating = Rating {dai    = Nothing, kaour = Nothing
                     ,eirlys = Nothing, elsie = Nothing}


instance Show Rating where
  show = prettyRating
