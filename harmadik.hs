import Data.Text.Array (resizeM)
import GHC.Unit.Database (DbModule (dbModuleName))
import System.Win32.DebugApi (Exception (ArrayBoundsExceeded))

myLength [] = 0
myLength (k : ve) = 1 + myLength ve

myLength1 [] = 0
myLength1 ls = 1 + myLength1 (tail ls)

myLength2 ls = aux ls 0
  where
    aux [] hossz = hossz
    aux ls hossz = aux (tail ls) (hossz + 1)

myLength2_v2 ls = aux ls 0
  where
    aux [] hossz = hossz
    aux (_ : ve) hossz = aux ve (hossz + 1) -- _ valami van, de nem nevesitjuk meg
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
myMinimum [k] = k -- ha csak egy elemu a lista, akkor a minimum elem maga az elem

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

myIndex :: [a] -> Int -> a
myIndex [] _ = error "Hiba"
myIndex ls 0 = head ls
myIndex ls i
  | i < 0 = error "Hiba"
  | otherwise = myIndex (tail ls) (i - 1)

myConcat :: [a] -> [a] -> [a]
myConcat [] ls2 = ls2
myConcat ls1 ls2 = head ls1 : myConcat (tail ls1) ls2

myConcate2 ls1 ls2
  | null ls1 = ls2
  | otherwise = myConcate2 (init ls1) (t : ls2)
  where
    t = last ls1

proba1 ls
  | null ls = []
  | otherwise = k : proba1 ve
  where
    k = head ls
    ve = tail ls

proba2 ls = aux ls []
  where
    aux ls rels
      | null ls = rels
      | otherwise = aux ve (k : rels)
      where
        k = head ls
        ve = tail ls

myInit ls
  | null ls = error "Hiba"
  | null ve = [] -- A bemeneti lista egy elembol all
  | otherwise = k : myInit ve
  where
    k = head ls
    ve = tail ls

myLast ls
  | null ls = error "Hiba"
  | null ve = k
  | otherwise = myLast ve
  where
    k = head ls
    ve = tail ls

palindrom ls
  | null ls = True
  | null ve = True
  | k == l = palindrom (init (tail ls))
  | otherwise = False
  where
    k = head ls
    l = last ls
    ve = tail ls

palindrom2 [] = True
palindrom2 [_] = True
palindrom2 (k : ve)
  | k == last ve = palindrom2 (init ve)
  | otherwise = False

palindrom3 ls = reverse ls == ls -- reverse fuggveny

myMove ls = tail ls ++ [head ls]

atlag1 [] = error "Hiba URES LISTA"
atlag1 ls = sum ls / fromIntegral (length ls)

atlag2 [] = error "Hiba URES LISTA"
atlag2 ls = osszeg / db
  where
    (osszeg, db) = atlag2' ls
    atlag2' ls
      | null ls = (0, 0)
      | otherwise = (osszeg + k, db + 1)
      where
        k = head ls
        ve = tail ls
        (osszeg, db) = atlag2' ve

atlag3 [] = error "Hiba URES LISTA"
atlag3 ls = osszeg / db
  where
    (osszeg, db) = atlag3' ls (0, 0)
    atlag3' ls (osszeg, db)
      | null ls = (osszeg, db)
      | otherwise = atlag3' ve (osszeg + k, db + 1)
      where
        k = head ls
        ve = tail ls

-- filter (>5) [4,6,10,9,11]
-- filter (/= 'a') "Sapienita"
-- filter odd [4,6,10,3,4]
-- atlag3 (filter (>5) [4,6,10,9,11])
-- (atlag3.filter (>5)) [4,6,10,9,11]

atlagFilter fg = atlag3 . filter fg

-- atlagFilter (>5)[5,4,41,5,66,6]

-- main :: IO ()
-- main = do
--   putStr ("")
--   print ()

parosNegyzet :: (Integral a) => Int -> [a]
parosNegyzet n = take n [x * x | x <- [0, 2 ..]]

-- [0,2..] a paros szamok vegtelen listaja

parosNegyzet1 :: (Integral a) => a -> [a]
parosNegyzet1 n = takeWhile (< n) [x * x | x <- [0, 2 ..]]

-- n-nel kisebbeket hatarozza meg

-- aze elso 1 2 2 3 3 3 4 4 4 4 ...

replicateSzamok :: Int -> [Int]
replicateSzamok n = take n (aux 1)
  where
    aux i = replicate i i ++ aux (i + 1)

-- igy az aux egy vegtelen fuggveny kiertekelest vegez

replicateParosSzamok n = take n (aux 1 2)
  where
    aux i j = replicate i j ++ aux (i + 1) (j + 2)

replicateParosSzamok1 n = take n (aux 1)
  where
    aux i = replicate i (2 * i) ++ aux (i + 1)

-- az első [n, n-1, ... 2, 1, 1, 2, ..., n-1, n]
fel1lab4 n = [n, n - 1 .. 1] ++ [1 .. n]

-- váltakozva tartalmazzon True és False értékeket

fellab4b n = take n aux
  where
    aux = [True, False] ++ aux

-- váltakozva tartalmazza a 0, 1, -1 értékeket

fellab4c n = take n aux
  where
    aux = [0, 1, -1] ++ aux

--  Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely
-- meghatározza egy adott szám osztóinak számát

osztokSzama a = length $ [x | x <- [1 .. div a 2], mod a x == 0] ++ [a]

-- lehet olyan feladat is, hogy 1 es n szamok kozul, amelyeknek a legtobb osztoja van
-- megoldas:
legtobbOszto n = (m, eredmeny)
  where
    m = maximum $ map osztokSzama [1 .. n]
    osztoTupleLista = map (\x -> (x, osztokSzama x)) [1 .. n]
    reszlegesEredmeny = filter (\(x, y) -> y == m) osztoTupleLista
    eredmeny = map fst reszlegesEredmeny

legtobbOszto1 n = (m, eredmeny)
  where
    proba i = proba (i + 1) -- hiaba irtuk ide be, ez nem kerul kiertekelesre
    osztokSzamaLista = map osztokSzama [1 .. n]
    m = maximum osztokSzamaLista
    reszlegesEredmeny = filter (\(x, y) -> y == m) osztoTupleLista
    eredmeny = map fst reszlegesEredmeny
    osztoTupleLista = zip [1 .. n] osztokSzamaLista

-- a do blokkbn egymas utan tortenik a kiertekeles
mainOsztok n = do
  let osztokSzamaLista = map osztokSzama [1 .. n]
  print osztokSzamaLista
  let m = maximum osztokSzamaLista
  -- print m
  let osztoTupleLista = zip [1 .. n] osztokSzamaLista
  let reszlegesEredmeny = filter (\(x, y) -> y == m) osztoTupleLista
  -- let eredmeny = map fst reszlegesEredmeny
  -- print m
  -- print eredmeny
  print m
  -- mapM_ print reszlegesEredmeny
  -- mapM_ (print . fst) reszlegesEredmeny
  mapM_ myPrint reszlegesEredmeny
  putStrLn ""
  where
    myPrint (x, y) = putStr $ show x ++ " "
