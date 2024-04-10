-- egy szám számjegyeinek szorzatát
szamSzamjegyeinekSzorzata :: Integral t => t -> t
szamSzamjegyeinekSzorzata number 
    | number < 10 = number 
    | otherwise = (number `mod` 10) * szamSzamjegyeinekSzorzata (number `div` 10)

-- egy szám számjegyeinek összegét
szamSzemjegyeinekOsszege number 
    | number < 10 = number
    | otherwise = (number `mod` 10) + szamSzemjegyeinekOsszege (number `div` 10)

-- egy szám számjegyeinek számát
szamSzamjegyeinekSzama number
    | number < 10 = 1
    | otherwise = 1 + szamSzamjegyeinekSzama (number `div` 10)

-- egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:
-- > fugv4 577723707 7
-- 35
fugv4 :: Integral t => t -> t -> t
fugv4 number szamjegy 
    | number < 10 = if number == szamjegy then number else 0
    | otherwise = (if (number `mod` 10) == szamjegy then number `mod` 10 else 0) + fugv4 (number `div` 10) szamjegy

-- egy szám páros számjegyeinek számát
parosSzamjegyekSzama :: Integral a => a -> Int
parosSzamjegyekSzama n
    | n < 10 = if paros n then 1 else 0
    | otherwise = (if paros (n `mod` 10) then 1 else 0) + parosSzamjegyekSzama (n `div` 10)
    where
        paros x = x `mod` 2 == 0

-- egy szám legnagyobb számjegyé
maxSzamjegy n
  | n < 10 = n
  | tmp1 > maxi = tmp1
  | otherwise = maxi
  where
    tmp1 = n `mod` 10
    tmp2 = n `div` 10
    maxi = maxSzamjegy tmp2

-- egy szám b számrendszerbeli alakjában a d-vel egyenlő számjegyek számát (például a b = 10-es számrendszerben a d = 2-es számjegyek száma),
-- Példák függvényhívásokra:
-- fugv 7673573 10 7 -> 3
-- fugv 1024 2 1 -> 1
-- fugv 1023 2 1 -> 10
-- fugv 345281 16 4 -> 2
fugv :: Integral a => a -> a -> a -> Int
fugv 0 _ _ = 0
fugv n base d
    | n `mod` base == d = 1 + fugv (n `div` base) base d
    | otherwise = fugv (n `div` base) base d


-- az 1000-ik Fibonacci számo
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


fibonacci1 :: Integral p => p -> p
fibonacci1 n = t1
  where
    (t1, t2) = aux n
    aux :: (Integral a) => a -> (a, a)
    aux n
      | n == 0 = (0, 1)
      | otherwise = (a + b, a)
      where
        (a, b) = aux (n - 1)

--tovabbi feladatok

--szam szamjegyeinek szorzata
szamszorzata2 n = max n 1
  where
    aux n res
      | n < 10 = res * n
      | otherwise = aux tmp2 (res * tmp1)
      where
        tmp1 = n `mod` 10
        tmp2 = n `div` 10

--kiszamoljuk ezzel az n-ik fibonacci szamot
fibonacci2 :: (Integral a) => a -> a
fibonacci2 n = aux n (0, 1)
  where
    aux :: (Integral a) => a -> (a, a) -> a
    aux n (t1, t2)
      | n == 0 = t1
      | otherwise = aux (n - 1) (t2, t1 + t2)

--fibonacci szam szamjegyeinek szamat adja ki
fibSzamjegyekszama :: (Show a, Integral a) => a -> Int
fibSzamjegyekszama n = length $ show $ fibonacci2 n


fibSzamjegyekszamaFG :: (Show a, Integral a) => a -> (a -> a) -> Int -- (a -> a) ez a fuggveny tipusa, figyelni erre! valamibol lesz valami
fibSzamjegyekszamaFG n fg = length $ show $ fg n
--meghivas:
--fibSzamjegyekszamaFG 15 fibonacci2


--10ik, 20ik, 300ik fibo szam szamjegyeinek a szama
fibSzamokSzJ :: (Show a, Integral a) => [a] -> [Int]
fibSzamokSzJ = map fibSzamjegyekszama 
--meghivas
-- fibSzamokSzJ [10, 20, 3000]

fibSzamok :: [Integer] -> [Integer]
fibSzamok = map fibonacci2
-- fibSzamok [10, 20, 30, 40, 50]
--x-ik fibo szamokat adja ki

--megadott listabol a szamok legnagyobb szamjegyeid adja ki
maxSzamjegyLista = map maxSzamjegy
--meghivas:
-- maxSzamjegyLista [123, 456, 789, 123456789]


--ugyanaz, mint a maxSzamjegyLista
maxSzamjegyLC :: Integral a => [a] -> [a]
maxSzamjegyLC ls = [maxSzamjegy x | x <- ls]


