import Distribution.PackageDescription (libVersionNumber)

-- splitAt 3 [1,2,3,4,5]
-- A splitAt függvény egy listát két részre bont egy adott indexnél.

-- 4 `notElem` [1,2,3]
-- True
-- A notElem függvény ellenőrzi, hogy egy elem nem szerepel-e egy listában

-- concat [[1,2],[3,4],[5,6]]
-- [1,2,3,4,5,6]
-- A concat függvény egy lista listáját egy egyszerű listává fűzi össze.

-- take 5 (repeat 3)
-- [3,3,3,3,3]
-- A repeat függvény egy elemet végtelen ideig ismétel.

-- replicate 3 'a'
-- "aaa"
-- A replicate függvény egy elemet egy adott számú alkalommal ismétel.

-- take 10 (cycle [1,2,3])
-- [1,2,3,1,2,3,1,2,3,1]
-- A cycle függvény egy listát végtelen ideig ismétel.

-- take 5 (iterate (* 2) 1)
-- [1,2,4,8,16]
-- Az iterate függvény egy függvényt alkalmaz egy kezdőértékre, és az eredményt a következő bemenetre adja, ezt addig ismételve, amíg a kívánt elemszámot el nem érjük.

-- any (> 5) [1,2,3,4,5]
-- False
-- Az any függvény ellenőrzi, hogy a lista legalább egy eleme kielégíti-e a megadott feltételt.

-- all (> 0) [1,2,3,4,5]
-- True
-- Az all függvény ellenőrzi, hogy a lista minden eleme kielégíti-e a megadott feltételt.

--------------------------------
-- II.
-- Alapvető függvények implementálása:
-- length
length' = foldl (\acc _ -> acc + 1) 0

-- sum
sum' = foldl (+) 0

-- elem
elem' x = foldl (\acc y -> if y == x then True else acc) False

-- reverse
reverse' = foldl (\acc x -> x : acc) []

-- product
product' = foldl (*) 1

-- maximum
maximum' = foldl (\acc x -> if x > acc then x else acc) minBound

-- insert-sort
insertSort' = foldr (\x acc -> insert' x acc) []
  where
    insert' x [] = [x]
    insert' x (y:ys) | x <= y = x:y:ys
                    | otherwise = y:insert' x ys

-- ++
(++) = flip (foldl (\acc x -> acc ++ [x]))

-- map
map' f = foldr (\x acc -> f x : acc) []

-- filter
filter' p = foldr (\x acc -> if p x then x:acc else acc) []


-- Pozitív elemek összegének meghatározása:
sumPositive = foldl (\acc x -> if x > 0 then acc + x else acc) 0

-- Páros elemek szorzatának meghatározása:
productEven = foldl (\acc x -> if even x then acc * x else acc) 1

-- n-ig a négyzetszámok:
squares n = [x^2 | x <- [1..n]]

-- Polinom értékének meghatározása:
evalPoly xs x0 = foldl (\acc a -> a + x0 * acc) 0 xs

------HASZNALAT-------

-- -- length'
-- Prelude> length' [1, 2, 3, 4, 5]
-- 5

-- -- sum'
-- Prelude> sum' [1, 2, 3, 4, 5]
-- 15

-- -- elem'
-- Prelude> elem' 3 [1, 2, 3, 4, 5]
-- True
-- Prelude> elem' 6 [1, 2, 3, 4, 5]
-- False

-- -- reverse'
-- Prelude> reverse' [1, 2, 3, 4, 5]
-- [5, 4, 3, 2, 1]

-- -- product'
-- Prelude> product' [1, 2, 3, 4, 5]
-- 120

-- -- maximum'
-- Prelude> maximum' [1, 2, 3, 4, 5]
-- 5

-- -- insertSort'
-- Prelude> insertSort' [5, 2, 4, 1, 3]
-- [1, 2, 3, 4, 5]

-- -- ++ 
-- Prelude> [1, 2, 3] ++ [4, 5]
-- [1, 2, 3, 4, 5]

-- -- map'
-- Prelude> map' (*2) [1, 2, 3, 4, 5]
-- [2, 4, 6, 8, 10]

-- -- filter'
-- Prelude> filter' (> 2) [1, 2, 3, 4, 5]
-- [3, 4, 5]

-- -- sumPositive
-- Prelude> sumPositive [-1, 2, -3, 4, 5]
-- 11

-- -- productEven
-- Prelude> productEven [1, 2, 3, 4, 5]
-- 24

-- -- squares
-- Prelude> squares 5
-- [1, 4, 9, 16, 25]

-- -- evalPoly
-- Prelude> evalPoly [1, 2, 3] 2
-- 29





-- III. 1.
-- Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza: function class Float higher-order monad tuple variable Maybe recursion akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
lsStr = words "function class Float higher-order monad tuple variable Maybe recursion"

-- shortestWordsInStr lsStr = (minLength,shortestWords)
--     where
--         -- the order of the expressions doesn't matter
--         lengthLs = map length lsStr
--         minLength = minimum lengthLs
--         tls = zip lsStr lengthLs
--         rls = filter ((t1, t2) -> t2 == minLength) tls
--         -- rls = filter (\t -> snd t == minLength) tls
--         shortestWords = map fst rls

-- main0 = do
--     let (m, ls) = shortestWordsInStr lsStr
--     print m
--     print ls

-- main1 = do
--     -- the order of the expressions matter
--     let lengthLs = map length lsStr
--     -- print lengthLs
--     let minLength = minimum lengthLs
--     let tls = zip lsStr lengthLs
--     let rls = filter ((t1, t2) -> t2 == minLength) tls
--     -- print rls
--     -- mapM print rls
--     putStrLn $ "minimum: " ++ show minLength
--     mapM myPrint rls
--     putStrLn ""
--         where
--             myPrint (t1, t2) = putStr $ t1++ " "

shortestWordsInStr :: String -> (Int, [String])
shortestWordsInStr str = (minLength, shortestWords)
  where
    lsStr = words str
    lengthLs = map length lsStr
    minLength = maybe 0 id (minimum lengthLs)
    tls = zip lsStr lengthLs
    rls = filter (\(t1, t2) -> t2 == minLength) tls
    shortestWords = map fst rls

main0 :: IO ()
main0 = do
  let (m, ls) = shortestWordsInStr "function class Float higher-order monad tuple variable Maybe recursion"
  print m
  print ls




-- III. 2.
-- lsNums = [10, 7, 8, 5, 3, 5, 2, 5]
-- el = 5
-- Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
-- Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.
-- > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
-- [2, 6, 8]
-- > talalat 'e' "Bigeri-vizeses"
-- [3,10,12]

(el, lsNums) = ('a', "sapientia")

main2 = do
    let zls = zip lsNums [1..]
    print zls
    let rls = filter (\x -> fst x == el) zls
    mapM_ (print . snd) rls

-- III. 3.
-- Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
-- Például:
-- > ls = [("golya",120),("fecske",85),("cinege",132)]
-- > osszegT ls
-- 337
ls = [("golya",120),("fecske",85),("cinege",132)]

main3 = do
    let valuesLs = map snd ls
    let sumLs = sum valuesLs
    print sumLs

ls2 = [("golya",120, 2),("fecske",85, 1),("cinege",132, 3)]

main = do
    -- let sumValuesLs = sum [mySnd t | t <- ls2]
    let sumValuesLs = sum $ map mySnd ls2
    let avg = sumValuesLs / fromIntegral(length ls2)
    print avg
        where
            mySnd (t1, t2, t3) = t2
