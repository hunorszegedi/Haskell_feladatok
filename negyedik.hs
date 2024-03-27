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


-- adott szam legnagyobb paratlan osztoja, ami nem egyenlo a szammal
keresParatlanOszto nr =  head [x | x <- [d, d -2 .. 1], mod nr x == 0]
    where 
        temp = div nr 2
        d = if even temp then temp -1 else temp

szamjegyekSzama1 nr = length $ show nr --10-es szamrendszerben

nrToBaseP nr p = aux nr p[]
    where 
        aux nr p ls 
            | nr == 0 = ls
            | otherwise = aux ujNr p (k:ls)
                where 
                    k = mod nr p 
                    ujNr = div nr p

feladat4 nr p = length $ nrToBaseP nr p