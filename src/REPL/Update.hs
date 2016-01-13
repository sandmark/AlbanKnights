module REPL.Update (update) where

import Rating

update :: Rating -> Either String Rating
update r = Right $ mapRating updateEach r
  where updateEach (Nothing, l) = (Nothing, l)
        updateEach (Just x, True) = (Just x, True)
        updateEach (Just x, False) = (Just $ up x, False)
          where up n = let n' = n + 3
                       in if n' > 97 then 1 else n'
