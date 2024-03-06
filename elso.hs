-- importok

import Data.Char
import Data.Data (mkIntegralConstr)
import Data.List
import System.Win32 (LOCALESIGNATURE (lsCsbDefault))

-- osszeadas fuggvenye
osszead :: Int -> Int -> Int
osszead x y = x + y

-- kivonas fuggvenye
kivonas :: Int -> Int -> Int
kivonas x y = x - y

-- fuggveny a szorzashoz
szorzas :: Int -> Int -> Int
szorzas x y = x * y

-- fuggveny a hanyadoshoz
hanyados :: Float -> Float -> Float
hanyados x y = x / y

-- fuggveny az osztasi maradekhoz
osztasiM :: Int -> Int -> Int
osztasiM x y = x `mod` y

-- elsofoku egyenlet gyoke
elsoFokuGyoke :: Float -> Float -> Float
elsoFokuGyoke x y = y / x

-- egy szam abszolut erteke
abszolutErtek :: Int -> Int
abszolutErtek x
  | x < 0 = -x
  | otherwise = x

-- egy szam elojele
egySzamElojele :: Int -> Char
egySzamElojele x
  | x < 0 = '-'
  | x == 0 = '0'
  | otherwise = '+'

-- ket argumentum kozul a maximum
maximuM :: Int -> Int -> Int
maximuM x y
  | x < y = y
  | x > y = x

-- ket argumentum kozul a minimum
miniM :: Int -> Int -> Int
miniM x y
  | x < y = x
  | x > y = y

-- masodfoku egyenlet gyoke
masodfokuEgyenlet :: Double -> Double -> Double -> (Double, Double)
masodfokuEgyenlet a b c = (x1, x2)
  where
    delta = b * b - 4 * a * c
    tempA = 2 * a
    x1 = (-b - sqrt delta) / tempA
    x2 = (-b + sqrt delta) / tempA

-- masodfoku egyenlet gyoke
masodfokuEgyenlet2 :: Double -> Double -> Double -> (Double, Double)
masodfokuEgyenlet2 a b c
  | delta >= 0 = (x1, x2)
  | otherwise = error "komplex gyokok"
  where
    -- \| otherwise = -- komplex gyokok

    tempA = 2 * a
    x1 = (-b - sqrt delta) / tempA
    x2 = (-b + sqrt delta) / tempA
    delta = b * b - 4 * a * c

-- nem muszaj a sorrendet betartani

-- masodf :: Double -> Double -> Double -> (Double, Double)
masodf :: (Ord t, Floating t) => t -> t -> t -> (t, t)
masodf a b c = if delta >= 0 then (x1, x2) else error "Komplex gyokok"
  where
    tempA = 2 * a
    x1 = (-b - sqrt delta) / tempA
    x2 = (-b + sqrt delta) / tempA
    delta = b * b - 4 * a * c

-- nem muszaj a sorrendet betartani

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:
-- az első n természetes szám negyzetgyökét
listaNegyzetgyok :: Double -> [Double]
listaNegyzetgyok n = [sqrt x | x <- [0 .. n]]

-- az első n négyzetszámot
listNegyzet :: Int -> [Int]
listNegyzet n = [x * x | x <- [0 .. n]]

-- az első n természetes szám köbét
listKob :: Int -> [Int]
listKob n = [x ^ 3 | x <- [0 .. n]]

listKob2 :: Double -> [Double]
listKob2 n = [x ** 3 | x <- [0 .. n]]

-- az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
listaNotSqrt :: Int -> [Int]
listaNotSqrt n = [1 .. n] \\ listNegyzet n

testSqrt a = temp * temp == a -- a fuggveny egy logikai kifejezes
  where
    temp = truncate (sqrt (fromIntegral a))

testSqrt2 :: (RealFrac a, Floating a) => a -> Bool
testSqrt2 a = t2 == 0
  where
    (t1, t2) = properFraction (sqrt a)

listaNotSqrt2 :: (Integral a) => a -> [a]
listaNotSqrt2 n = [x | x <- [1 .. n], not (testSqrt x)]

listaSqrt3 :: (Floating a1, RealFrac a1, Integral a2, Enum a1) => a1 -> [a2]
listaSqrt3 n = [truncate x | x <- [1 .. n], testSqrt2 x]

listaNotSqrt3 :: (Enum a, RealFrac a, Floating a) => a -> [a]
listaNotSqrt3 n = [x | x <- [1 .. n], not (testSqrt2 x)]

-- x hatványait adott n-ig,
listXHatvanyaiAdottNig x n = [x ^ i | i <- [0 .. n]]

-- egy szam paros osztoinak szama
lisSzamParosOsztoinakSzama x = [i | i <- [2 .. x], if x `mod` i == 0 then True else False]

listaParosOsztoinakSzama2 x = [i | i <- [2, 4 .. x], x `mod` i == 0]

listaParosOsztoinakSzama3 x = [i | i <- [2, 4 .. x], mod x i == 0] -- ez a legegyszerubb

-- n-ig a primszamok listaja
listaNIgPrimszamokListaja n = 2 : [x | x <- [3, 5 .. n], teszPrimszam x 3] -- beteszem a 2-est az elejere
-- where
-- tesztPrimszam nr

teszPrimszam nr i
  | i * i > nr = True
  | mod nr i == 0 = False
  | otherwise = teszPrimszam nr (i + 1)

-- osszetettSzamokListaja

-- $ al megszabadulunk a zarojelektol eloszor

-- $ utani kifejezes elsokent ertekelodik ki

listaOsszetettSzamokListaja n = sort $ [4, 6 .. n] ++ [x | x <- [3, 5 .. n], not (teszPrimszam x 3)]
  where
    tesPrimszam nr i
      | i * i > nr = True
      | mod nr i == 0 = False
      | otherwise = teszPrimszam nr (i + 1)

-- a következő listát: [('a',0), ('b',1),..., ('z', 25)],
-- mindent mindennel parosit [(i,j) | i <- ['a'..'z'], j <- [0..25]]
kovetkezoLista = zip ['a' .. 'z'] [0 .. 25]

ovetkezoLista2 = [(i, ord i - 97) | i <- ['a' .. 'z']]

-- 3.ik labor

-- ez a megoldas mar le van irva
primeNumbers n
  | n < 0 = error "Hiba"
  | otherwise = 2 : [x | x <- [3, 5 .. n], isPrime x 3]
  where
    isPrime :: (Integral a) => a -> a -> Bool
    isPrime n i -- paratlan szamok tesztelesekor ok
      | i * i > n = True
      | mod n i == 0 = False -- n `mod` i
      | otherwise = isPrime n (i + 2)

compositeNumbers n
  | n < 0 = error "Hiba"
  | otherwise = [1 .. n] \\ primeNumbers n

isComposite :: (Integral a) => a -> Bool
isComposite n = not (isPrime n 2)
  where
    isPrime :: (Integral a) => a -> a -> Bool
    isPrime n i
      | i * i > n = True
      | mod n i == 0 = False
      | otherwise = isPrime n (i + 1)

compositeNumbers2 :: (Integral a) => a -> [a] -- osszetett szamok listaja
compositeNumbers2 n
  | n < 0 = error "Hiba"
  | otherwise = 1 : [x | x <- [1 .. n], isComposite x]

pitagorasNumbers :: (Integral a) => a -> [(a, a, a)]
-- Pitagoraszi szamharmas
pitagorasNumbers n = [(x, y, z) | x <- [1 .. n], y <- [x + 1 .. n], z <- [y + 1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

listaGeneralas a = [(x, a - x) | x <- [a, a - 1 .. 0]]

-- True False True False ertekeket tartalmazo lista
trueFalseLista n
  | n == 0 = []
  | even n = True : trueFalseLista (n - 1)
  | otherwise = False : trueFalseLista (n - 1)

trueFalseLista2 n ls
  | n == 0 = ls
  | even n = trueFalseLista2 (n - 1) (True : ls)
  | otherwise = trueFalseLista2 (n - 1) (False : ls)

listaGeneralas5 n = [even x | x <- [1 .. n]]

listaGeneralas5_ n = [myFunc x | x <- [1 .. n]]
  where
    myFunc x
      | even x = True
      | otherwise = False

listaGeneralas6 n = [myFunc x | x <- [1 .. n]]
  where
    myFunc x
      | mod x 3 == 0 = "Info"
      | mod x 3 == 1 = "Szamtech"
      | otherwise = "Tavk"

main :: IO ()
main = do
  putStr ("Osszeg: ")
  print (osszead 2 3)
  putStr ("Kulonbseg: ")
  print (kivonas 3 2)
  putStr ("Szorzas: ")
  print (szorzas 4 4)
  putStr ("Hanyados: ")
  print (hanyados 5 4)
  putStr ("Osztasi maradek: ")
  print (hanyados 5 4)

  putStr ("elsofoku egyenlet gyoke: ")
  print (elsoFokuGyoke 2 3)
  putStr ("egy szam abszolut erteke: ")
  print (abszolutErtek (-3))

  putStr ("egy szam elojele: ")
  print (egySzamElojele (-4))
  putStr ("egy szam elojele: ")
  print (egySzamElojele 4)

  putStr ("Maximum: ")
  print (maximuM 2 3)
  putStr ("Maximum: ")
  print (maximuM 4 3)

  putStr ("Minimum: ")
  print (miniM 2 3)
  putStr ("Minimum: ")
  print (miniM 4 3)

  putStr ("Masodfoku egyenlet 1: ")
  print (masodfokuEgyenlet 1 2 1)
  putStr ("Masodfoku egyenlet 2: ")
  print (masodfokuEgyenlet2 1 2 1)
  putStr ("Masodfoku egyenlet 3: ")
  print (masodf 1 2 4)
