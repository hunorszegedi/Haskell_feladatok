import Data.Char
import Data.List
import Data.Ord
import System.IO 
import Data.Bits

data Fesztivalok = Fesztivalok {
    fEgyuttes :: String,
    fFesztival :: String,
    fAr :: Int,
    fKod :: Int
} deriving (Show)

lsFeszt = [
    Fesztivalok "Slipknot" "Halloween" 1000 1,
    Fesztivalok "Jason Derulo" "Vibe" 350 2, 
    Fesztivalok "Linkin Park" "Untold" 500 3,
    Fesztivalok "Khalid" "Bibe" 2000 4]

valogat1 fesztNev ls = filter (\x -> fEgyuttes x == fesztNev) ls 

myPrint x = putStrLn (fEgyuttes x)

main1 = do 
    putStrLn "Add meg a fesztival nevet:"
    fesztNev <- getLine 
    let resls = valogat1 fesztNev lsFeszt
    mapM_ myPrint resls
    putStrLn "Olcsobb egyuttesek"
    let resLs1 = valogat2 600 lsFeszt
    mapM_ myPrint resLs1

valogat2 ar ls = filter (\x -> fAr x < ar) ls

beolvasFile fileNev = do 
    inputFile <- openFile fileNev ReadMode
    temp <- hGetContents inputFile
    let ls = lines temp 
        lsFeszt = map mapFg ls 
    hClose inputFile
    return lsFeszt  

beolvasFile2 fileNev = do 
    inputFile <- openFile fileNev ReadMode  
    temp <- hGetContents inputFile
    let ls = lines temp 
        lsFeszt = map mapFg ls 
    hClose inputFile
    return lsFeszt

myPrint2 outputFile feszt =
    hPutStrLn outputFile $ fEgyuttes feszt ++ " " ++ 
        fFesztival feszt ++ " " ++ show (fAr feszt) ++ show (fKod feszt)

mapFg str = Fesztivalok egyuttes fesztival ar kod 
    where
        temp = words str
        egyuttes = temp !! 0
        fesztival = temp !! 1
        ar = read (temp !! 2) :: Int
        kod = read (temp !! 3) :: Int


mainRendez :: IO ()
mainRendez = do 
    lsFeszt <- beolvasFile2 "fesztivalok.txt"
    let rendezettFeszt = sortOn fEgyuttes lsFeszt
    outputFile <- openFile "rendezettFeszt.txt" WriteMode
    mapM_ (myPrint2 outputFile) rendezettFeszt 
    hClose outputFile




--masik feladat
type Hash = Integer

hash :: String -> Hash    
hash str = foldr op 0 $ zip str [1..]
    where
        op (c,i) res = res + i * ( shiftL 1 (ord c))


type Salt = String 
type KDF = Salt -> String -> String

kdf1 :: KDF 
kdf1 salt password = password ++ salt ++ reverse password

kdf2 :: Int -> KDF 
kdf2 hossz salt password
    | null salt = take hossz $ cycle password
    | otherwise = take hossz $ password ++ cycle salt 

data Password = P Hash Salt 
    deriving (Show)

mkPassword :: DKF -> Salt -> String -> Password
mkPassword kdf salt password  = P hashV salt 
    where 
        hashV  = hash $ kdf salt password