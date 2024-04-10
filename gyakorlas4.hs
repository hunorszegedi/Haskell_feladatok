import Numeric (showIntAtBase)
import Data.Char (intToDigit, digitToInt)
import Data.List (group, maximumBy)
import Data.Function (on)

--az első n páros szám négyzetét
elsoNParoSzam n = take n [i*i | i <- [0,2..]]

--az első [1, 2, 2, 3, 3, 3, 4, 4, 4, 4,...]
replicateSzamok :: Int -> [Int]
replicateSzamok n = take n (aux 1)
  where
    aux i = replicate i i ++ aux (i + 1)

-- Az első [2, 4, 4, 6, 6, 6, 8, 8, 8, 8,...]
replicateSzamok2 :: Int -> [Int]
replicateSzamok2 n = take n (aux 1)
  where
    aux i = replicate i i ++ aux (i + 1)

-- Az első [n, n-1, ..., 2, 1, 1, 2, ..., n-1, n]
descendingAscending :: Int -> [Int]
descendingAscending n = concat [ [n,n-1..1], [1..n] ]

-- Váltakozva tartalmazza True és False értékeket
alternatingBools :: Int -> [Bool]
alternatingBools n = concatMap (\x -> [odd x]) [1..n]

-- Váltakozva tartalmazza a 0, 1, -1 értékeket
alternatingInts :: Int -> [Int]
alternatingInts n = concatMap (\x -> if even x then [0, -1] else [1]) [1..n]

-- Meghatározza egy adott szám osztóinak számát
osztoSzam :: Int -> Int
osztoSzam n = length [x | x <- [1..n], n `mod` x == 0]

-- Meghatározza egy adott szám legnagyobb páratlan osztóját
legnagyobbParatlanOszto :: Int -> Int
legnagyobbParatlanOszto n = maximum [x | x <- [1..n], n `mod` x == 0, odd x]

-- Meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz
szamjegyekSzama :: Int -> Int -> Int
szamjegyekSzama szam p = length $ showIntAtBase p intToDigit szam ""

-- Meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy
legnagyobbSzamjegy :: Int -> Int -> Int
legnagyobbSzamjegy szam p = maximum $ map digitToInt $ showIntAtBase p intToDigit szam ""

-- Meghatározza az a és b közötti Fibonacci számokat, a > 50
fibonacci :: Int -> [Int]
fibonacci a = takeWhile (\x -> x <= a) $ dropWhile (<= 50) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Meghatározza egy lista pozitív elemeinek átlagát
pozitivAtlag :: (Fractional a, Ord a) => [a] -> a
pozitivAtlag xs = sum' / fromIntegral count
  where
    positives = filter (>0) xs
    sum' = sum positives
    count = length positives

-- Meghatározza azt a listát, amely tartalmazza az eredeti lista minden n-edik elemét
everyNthElement :: Int -> [a] -> [a]
everyNthElement n xs = map snd $ filter (\(i, _) -> i `mod` n == 0) $ zip [1..] xs

-- Két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy, hogy a lista elemeit csak egyszer járja be
maxIndices :: Ord a => [a] -> [Int]
maxIndices xs = indices1 ++ indices2
  where
    maxVal = maximum xs
    indices1 = findIndices (\x -> x == maxVal) xs
    indices2 = findIndices (\x -> x == maxVal) $ reverse xs
    findIndices p = map fst . filter (p . snd) . zip [0..]

-- Meghatározza egy lista leggyakrabban előforduló elemét
mostFrequent :: Eq a => [a] -> a
mostFrequent [] = error "Empty list"
mostFrequent xs = head $ maximumBy (compare `on` length) $ group xs