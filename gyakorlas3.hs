-- egy szamsorozat atlagat adja vissza
atlag :: (Fractional a, Foldable t) => t a -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- meghivas:
-- atlag[3, 4, 5, 9]

-- egy szamszorozat elemszamat adja meg
elemszam :: (Foldable t) => t a -> Int
elemszam ls = length ls

--ugyanaz
myLength [] = 0
myLength (k : ve) = 1 + myLength ve

--lista elemeneineks szorzata
myProduct :: Num a => [a] -> a
-- myProduct [] = 1
myProduct ls
    | length ls == 1 = head ls
    | otherwise = head ls * myProduct (tail ls)
--meghivas:
--myProduct [2,4,5]

--ugyanaz
myProduct2 :: Num a => [a] -> a
myProduct2 [] = 1
myProduct2 (x:xs) = x * myProduct xs

--lista legkisebb elemenek meghatarozasa
myMinimum :: Ord a => [a] -> a
myMinimum [elem] = elem 
myMinimum (a:z)
    | a < minimumMaradekListaLegkisebbEleme = a
    | otherwise = minimumMaradekListaLegkisebbEleme
    where 
        minimumMaradekListaLegkisebbEleme =  myMinimum z
--meghivas:
--myMinimum [3,4,5,9,2,1,4,1]

--lista maximum elemenek meghatarozasa
myMaximum :: Ord a => [a] -> a
myMaximum [elem] = elem 
myMaximum (a:z)
    | a > listaMaxeleme = a 
    | otherwise = listaMaxeleme 
    where 
        listaMaxeleme = myMaximum z
--meghivas:
--myMaximum [512,412312,4,412]

--lista n-ik elemenek meghatarozasa
myIndex :: [a] -> Int -> a
myIndex [] _ = error "Hiba"
myIndex ls 0 = head ls
myIndex ls i
  | i < 0 = error "Hiba"
  | otherwise = myIndex (tail ls) (i - 1)
--meghivas:
--myIndex [9, 4, 3, 1, 3] 2

--egymasutan fuzi a parameterkent megadott ket listat
myConcat :: [a] -> [a] -> [a]
myConcat [] ls2 = ls2
myConcat ls1 [] = ls1
myConcat ls1 ls2 = head ls1 : myConcat (tail ls1) ls2

--lista palindrom-e
palindrom2 [] = True
palindrom2 [_] = True
palindrom2 (k : ve)
  | k == last ve = palindrom2 (init ve)
  | otherwise = False
--meghivas:
-- palindrom2 [1,2,3,3,2,1]

-- egesz szam szamjegyeinek a szama
szamjegyekSzama number 
    | number < 10 = 1
    | otherwise = 1 + szamjegyekSzama (number `div` 10)

-- ghci> head [1,2,3,4]
-- 1
-- ghci> tail [1,2,3,4]
-- [2,3,4]
-- ghci> init [1,2,3,4]
-- [1,2,3]
-- ghci> last [1,2,3,4]
-- 4

--lista elso elemet elkoltozteti a vegere
-- 1 2 3 4 --> 2 3 4 1
listaElsoElemKoltoztet (a:z) = z ++ [a]
--meghivas:
-- listaElsoElemKoltoztet [1,2,3,4]
-- [2,3,4,1]


--egesz elemu lista elemeinek atlagerteke
atlagErteklista :: Fractional a => [a] -> a
atlagErteklista ls = sum ls / fromIntegral (length ls)

--meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját
toBase :: Integral a => a -> a -> [a]
toBase 0 _ = [0]
toBase n base = reverse $ toBase' n base
  where
    toBase' 0 _ = []
    toBase' num b = r : toBase' q b
      where
        (q, r) = num `divMod` b
--meghivas
--toBase 12 2

--MAP
-- map (\x -> toBase x 2) [12,11,10,4,2]
-- valami = [toBase i 2 | i <- [12,11,10,4,2]] 

-- meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot
fromBase :: Integral a => [a] -> a -> a
fromBase digits base = foldl (\acc digit -> acc * base + digit) 0 digits
--meghivas:
-- fromBase [1,1,0,0,0,1] 2

main :: IO ()
main = do
--   putStr ("Atlag: ")
--   print (atlag [4, 5, 9, 10])
--   putStr ("Elemszamai: ")
--   print (elemszam [3, 3, 5, 5, 6])
--   print ((atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2])
--   --a 4.5-nel nagyobb vagy egyenlo szamokra alkalmazza az atlagszamitasos fuggvenyt
--   print( atlag $ filter (<7)[11, 55, 2, 4, 6, 4, 99])
--   --hasonlo csak a zarojel kiszuresre kerul
--   print( (take 6 .reverse.filter even)[1..50])
--   --utiolso hat paros szam
--   print ( take 3.reverse.filter odd $ [1..30])
--   --utolso 3 paratlan szam
--   print ( take 3 $ reverse $ filter odd $ [1..40])
--   --utolso 3 paratlan szam
    let myList = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
    let result = myMinimum myList
    putStrLn $ "A lista legkisebb eleme: " ++ show result