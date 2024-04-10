import System.Win32 (COORD (xPos, yPos), aCCESS_SYSTEM_SECURITY, xBUTTON1)
import Data.Char
import Data.Data (mkIntegralConstr)
import Data.List
import System.Win32 (LOCALESIGNATURE (lsCsbDefault))

-- osszeadas fuggvenye
osszead :: (Num a) => a -> a -> a
osszead x y = x + y

kivonas :: (Num a) => a -> a -> a
kivonas x y = x - y

szorzat :: (Num a) => a -> a -> a
szorzat x y = x * y

hanyados :: (Integral a) => a -> a -> a
hanyados x y = x `div` y

maradek :: (Integral a) => a -> a -> a
maradek x y = x `mod` y

elsoFokuEgyenletGyoke x a
  | x == 0 = error "Nem masodfoku egyenlet"
  | otherwise = -a / x

szamAbszolutErteke :: (Ord a, Num a) => a -> a
szamAbszolutErteke x
  | x < 0 = -x
  | otherwise = x

elojel :: (Ord a, Num a) => a -> Char
elojel x
  | x < 0 = '-'
  | otherwise = '+'

ketArgumentumKozulMax :: (Ord a) => a -> a -> a
ketArgumentumKozulMax x y
  | x > y = x
  | x < y = y
  | otherwise = error "a ket szam egyenlo"

-- minimum ugyanaz

masodFokuGyokei :: (Ord b, Floating b) => b -> b -> b -> (b, b)
masodFokuGyokei a b c
  | delta >= 0 = (x1, x2)
  | otherwise = error "Hiba"
  where
    delta = b * b - 4 * a * c
    x1 = (-b + sqrt (delta)) / (2 * a)
    x2 = (-b - sqrt (delta)) / (2 * a)

ketElemparErtekeiMegegyeznek :: (Eq a) => (a, a) -> (a, a) -> Bool
ketElemparErtekeiMegegyeznek (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2 = True
  | x1 == y2 && y1 == x2 = True
  | otherwise = False

nSzamFaktorialisa n
  | n == 0 = 1
  | otherwise = n * nSzamFaktorialisa (n - 1)

xSzamNikHatvanya :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
xSzamNikHatvanya x 0 = 1
xSzamNikHatvanya x n = x * xSzamNikHatvanya x (n - 1)

hatvayn :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
hatvayn x 0 = 1
hatvayn x n = x * hatvayn x (n - 1)

valami :: (Enum a, Num a) => a -> [a]
valami n = [i * i | i <- [1 .. n]]

elsoNNegyzetgyoke :: (Enum a, Floating a) => a -> [a]
elsoNNegyzetgyoke n = [sqrt i | i <- [1 .. n]]

elsoNNegyzetszam :: (Enum a, Num a) => a -> [a]
elsoNNegyzetszam n = [i * i | i <- [1 .. n]]

elsoNTermeszetSzamKobe :: (Enum a, Floating a) => a -> [a]
elsoNTermeszetSzamKobe n = [i ** 3 | i <- [1 .. n]]

istestSqrt a = temp * temp == a -- a fuggveny egy logikai kifejezes
  where
    temp = truncate (sqrt (fromIntegral a))

isSQRT :: (Integral a) => a -> Bool
isSQRT a = temp * temp == a
  where
    temp = truncate (sqrt (fromIntegral a))

nincsenekNegyzetszamok :: (Integral a) => a -> [a]
nincsenekNegyzetszamok n = [i | i <- [0 .. n], not (isSQRT i)]

xhatvanyaiAdottNig :: (Enum a, Floating a) => a -> a -> [a]
xhatvanyaiAdottNig x n = [x ** i | i <- [1 .. n]]

isParos :: (Integral a) => a -> Bool
isParos n
  | n `mod` 2 == 0 = True
  | otherwise = False

szamParosOsztoiListaja :: (Integral a) => a -> [a]
szamParosOsztoiListaja number = [i | i <- [1 .. number], number `mod` i == 0, isParos i]

testPrimeHelper :: Integral a => a -> a -> Bool
testPrimeHelper nr i
  | i * i > nr = True
  | nr `mod` i == 0 = False
  | otherwise = testPrimeHelper nr (i + 1)

nigPrimszaokListaja :: Integral a => a -> [a]
nigPrimszaokListaja n = 2: [i | i <- [3, 5..n], testPrimeHelper n i]

teszPrimszam nr i
  | i * i > nr = True
  | mod nr i == 0 = False
  | otherwise = teszPrimszam nr (i + 1)

listaOsszetettSzamokListaja n = sort $ [4, 6 .. n] ++ [x | x <- [3, 5 .. n], not (teszPrimszam x 3)]
  where
    tesPrimszam nr i
      | i * i > nr = True
      | mod nr i == 0 = False
      | otherwise = teszPrimszam nr (i + 1)

--a következő listát: [('a',0), ('b',1),..., ('z', 25)],
listageneralas1 = [(betu,szam) | betu <- ['a'..'z'], szam <- [0..25]]
listageneralas2 :: [(Char, Int)]
listageneralas2 = zip ['a'..'z'] [0..25]
listageneralas3 :: [(Char, Int)]
listageneralas3 = [(betu, index) | (index, betu) <- zip [0..25] ['a'..'z']]
--a következő listát: [(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)], majd általánosítsuk a feladatot.

listageneralas4 :: (Enum b, Num b) => b -> b -> [(b, b)]
listageneralas4 n1 n2 = [(i, j) | (i, j) <- zip [n1..n2] [n2,n2-1..n1]]

--azt a listát, ami felváltva tartalmaz True és False értékeket.
-- True False True False ertekeket tartalmazo lista
trueFalseLista n
  | n == 0 = []
  | even n = True : trueFalseLista (n - 1)
  | otherwise = False : trueFalseLista (n - 1)

main :: IO ()
main = do
  let szam = -5
  putStr ("Szam abszolut erteke: ")
  print (szamAbszolutErteke szam)

