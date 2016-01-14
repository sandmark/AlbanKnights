module REPL.Update (update) where

import Rating

update :: Rating -> Either String Rating
update r = Right $ mapRating updateEach r
  where updateEach (Nothing, l, st) = (Nothing, l, st)
        updateEach (Just x, True, st) = (Just x, True, st)
        updateEach (Just x, False, st) = (Just $ up x, False, st)
          where up n = let n' = n + 3
                       in if n' > 97 then 1 else n'
