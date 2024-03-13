myLength  [] = 0
myLength  (k : ve) = 1 + myLength ve

myLength1 [] = 0
myLength1 ls = 1 + myLength1 (tail ls)

myLength2 ls = aux ls 0
  where
    aux [] hossz = hossz
    aux ls hossz = aux (tail ls) (hossz + 1)

myLength2_v2 ls = aux ls 0
  where
    aux [] hossz = hossz
    aux (_ : ve) hossz = aux ve (hossz + 1) --_ valami van, de nem nevesitjuk meg
-- _ mindenes operator, nem nevesitjuk meg


myProduct [] = 1
myProduct (k : ve) = k * myProduct ve

-- main :: IO ()
-- main = do
--   putStr ("")
--   print ()