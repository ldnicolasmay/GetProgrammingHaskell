-- lesson10.1_cup.hs

-- 10.1

-- -- closure that stores current flOz in the cup
-- cup flOz = \_ -> flOz

-- constructor for a basic `cup` object
cup :: (Num a) => a -> (a -> b) -> b
cup flOz = \message -> message flOz
-- The above could also be expessed as this:
-- cup flOz message = message flOz

-- getter
-- getOz :: ???
getOz aCup = aCup (\flOz -> flOz)

-- setter
-- setOz aCup flOz =

-- drink
-- drink aCup ozDrank = cup (flOz - ozDrank)
--   where flOz = getOz aCup
drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
  where flOz = getOz aCup
        ozDiff = flOz - ozDrank
      
isEmpty aCup = getOz aCup == 0


