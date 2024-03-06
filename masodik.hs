import qualified Control.Applicative as A
-- 1.feladat
-- Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:
-- egy szám számjegyeinek szorzatát (2 módszerrel)

szamSzamjegyeinekSzozata :: Int -> Int
szamSzamjegyeinekSzozata szam = if szam < 10 then szam else (szam `mod` 10) * szamSzamjegyeinekSzozata (szam `div` 10)

-- mod maradekot ad vissza
-- div egesz reszt ad vissza

szamSzamjegyeinekSzozata2 :: Int -> Int
szamSzamjegyeinekSzozata2 szam
  | szam < 10 = szam
  | otherwise = (szam `mod` 10) * szamSzamjegyeinekSzozata2 (szam `div` 10)

-- 2.feladat
-- egy szam szamjegyeinek osszeget 2 modszerrel
szamSzemjegyeinekOsszege :: Int -> Int
szamSzemjegyeinekOsszege szam
  | szam < 10 = szam
  | otherwise = (szam `mod` 10) + szamSzemjegyeinekOsszege (szam `div` 10)
--masik modszer
szamSzemjegyeinekOsszege2 :: Int -> Int
szamSzemjegyeinekOsszege2 szam = if szam < 10 then szam else (szam `mod` 10) + szamSzemjegyeinekOsszege2 (szam `div` 10)

-- 3.feladat
-- egy szam szamjegyeinek szamat
szamSzamjegyeinekSzama :: Int -> Int
szamSzamjegyeinekSzama szam
  | szam < 10 = 1
  | otherwise = 1 + szamSzamjegyeinekSzama (szam `div` 10)
--masik modszer
szamSzamjegyeinekSzama2 :: Int -> Int
szamSzamjegyeinekSzama2 szam = if szam < 10 then 1 else 1 + szamSzamjegyeinekSzama2 (szam `div` 10)

--4.feladat
-- egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:
-- > fugv4 577723707 7
-- 35
szamAzonSzamjegyeinekOsszege :: Int -> Int -> Int
szamAzonSzamjegyeinekOsszege szam szamjegy 
  | szam < 10 = if szam == szamjegy then szam else 0
  | otherwise = if (szam `mod` 10) == szamjegy then (szam `mod` 10) + szamAzonSzamjegyeinekOsszege (szam `div` 10) szamjegy else szamAzonSzamjegyeinekOsszege (szam `div` 10) szamjegy

--labor 3 



main :: IO ()
main = do
  putStr ("Szam szamjegyeinek szorzata: ")
  print (szamSzamjegyeinekSzozata 1234)
  putStr ("Szam szamjegyeinek szorzata2: ")
  print (szamSzamjegyeinekSzozata2 1234)
  putStr ("Szam szamjegyeinek osszege: ")
  print (szamSzemjegyeinekOsszege 1234)
  putStr("Szam szamjegyeinek szama: ")
  print (szamSzamjegyeinekSzama 1234)
