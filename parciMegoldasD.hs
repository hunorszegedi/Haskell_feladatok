import Data.List
-- 1-es feladat
listaSzavak = ["sapi", "hello", "szia", "kutya", "macska", "kacsa"];

main = do
    let halmazLista = myNubFoldl listaSzavak
    myPrint1 halmazLista
    -- myNub listaSzavak

myNub1 ls = myNub' ls []
    where
        myNub' ls ujLista
            | null ls = ujLista
            | elem k ujLista = myNub' ve ujLista
            | otherwise = myNub' ve ujLista ++ [k] --nem hatekony
            -- | otherwise = myNub' ve (k:ujLista)
                where 
                    k = head ls
                    ve = tail ls

myNub ls
    | null ls = []
    | elem k ujLista = ujLista
    | otherwise = k:ujLista
        where
            k = head ls
            ve = tail ls
            ujLista = myNub ve

myNubFoldl ls = foldl op [] ls
    where
        op ujLista k = if elem k ujLista then ujLista else k : ujLista

myNubFoldr ls = foldr op [] ls 
    where
        op k ujLista = if elem k ujLista then ujLista else k : ujLista

myPrint ls = putStrLn $ unwords ls

-- intercalate -- mit csinal: osszefuzi a lista elemeit egy szoveggel
-- unwords -- mit csinal: osszefuzi a lista elemeit szokozzel
-- words -- mit csinal: szetszedi a szoveget szavakra

myPrint1 ls = putStrLn $ intercalate "\n" ls

-- 2-es feladat
autokls = [("VW", 2017, 2000, 17), ("Audi", 2018, 2500, 20), ("BMW", 2019, 3000, 25), ("VW", 2020, 3500, 30), ("Opel", 2021, 4000, 35), ("VW", 2019, 2500, 20), ("Fiat", 2020, 3000, 25), ("Toyota", 2021, 3500, 30), ("Suzuki", 2018, 2000, 17), ("Skoda", 2017, 2500, 20)];
--kiirni formazva egy adott evben az autok adatait
felA ls ev = filter (fg ev) ls 
    where
        fg ev (x, y, z, w) = y == ev

main2 = do 
    let autok = felA autokls 2019
    mapM_ myPrintAutok autok

myPrintAutok (x, y, z, w) = putStrLn $ x ++ ": " ++ show z ++ " " ++ show w

-- 3-as feladat
--adott autobol hanyat adtak el es mennyi volt az osszbevetel

felB ls autoNev = filter (fg autoNev) ls
    where
        fg autoNev (x, y, z, w) = x == autoNev
        
main3 = do 
    let autok = felB autokls "VW"
    let db = length autok
    let osszBevetel = sum $ map (\(x, y, z, w) -> z) autok
    putStrLn $ "Az autok szama: " ++ show db
    putStrLn $ "Az osszbevetel: " ++ show osszBevetel

main4 = do 
    let autok = felB autokls "VW"
    let (db, osszBevetel) = foldr op (0, 0) autok
    -- print db 
    -- print osszBevetel
    putStrLn $ "Az autok szama: " ++ show db
    putStrLn $ "Az osszbevetel: " ++ show osszBevetel
        where 
            op (x, y, z, w) (db, b) = (db + w, b + z)

    