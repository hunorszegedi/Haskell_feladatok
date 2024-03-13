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

-- 1.verzio
myProduct [] = 1
myProduct (k : ve) = k * myProduct ve

-- 2.verzio
myProduct2 ls = aux ls 1
  where
    aux [] szorzat = szorzat
    aux (k : ve) szorzat = aux ve (szorzat * k)

myMinimum [] = error "Ures lista"
myMinimum [k] = k --ha csak egy elemu a lista, akkor a minimum elem maga az elem
muMinimum (k : ve)
    | k < mini = k
    | otherwise = mini
    where
        mini = myMinimum ve


myMinimum2 [] = error "HIBA"
myMinimum2 (k : ve) = aux k ve
  where
    aux m [] = m
    aux m (k : ve)
        | k < m = aux k ve
        | otherwise = aux m ve

myMinimum2_v2 ls
    | null ls = error "Hiba"
    | otherwise = aux (head ls) (tail ls)
    where
        aux m ls 
            | null ls = m
            | m > k = aux k ve
            | otherwise = aux m ve
                where 
                    k = head ls
                    ve = tail ls

-- main :: IO ()
-- main = do
--   putStr ("")
--   print ()