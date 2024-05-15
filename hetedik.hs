import Numeric
import System.IO
import System.IO (IOMode (WriteMode))

-- lucky numbers 1.verzio

szita (k : ve) p = k : szita [m | (i, m) <- zip [p ..] ve, i `mod` k > 0] (p + 1)

foSzita n = take n $ 1 : szita [3, 5 ..] 3

-- lucky numbers 2 verzio
mySelect ls n = [snd x | x <- filter (fgFilter n) $ zip [1 ..] ls]
  where
    fgFilter n (i, nr) = mod i n /= 0

luckyNr n = 1 : (take n $ rek 2 [1, 3 ..])

rek i tls = e : rek (i + 1) ls
  where
    e = tls !! (i - 1)
    ls = mySelect tls i

-- allomanykezeles

mainLucky = do
  outf <- openFile "lucky.txt" WriteMode
  putStr "n:"
  temp <- getLine
  let n = read temp :: Int
  let ls = foSzita n
  mapM_ (myPutstr outf) ls
  hClose outf

myPutstr outf nr = hPutStrLn outf $ show nr

-- Írjunk egy-egy Haskell függvényt, amely szövegállományban levő számokat olvas be
-- egy listába, és kiírja formázva egy másik szövegállományba
-- a számokkal együtt a számok 2-es, 16-os számrendszerbeli alakját,
-- illetve, hogy hány egyes szerepel a 2-es számrendszerbeli alakban

mainHexa = do
  putStr "szamok:"
  temp <- getLine
  let ls = map (read :: String -> Int) $ words temp
  print ls
  let lsHex = map (flip showHex "") ls
  print lsHex
  let lsBin = map (flip showBin "") ls
  print lsBin

  outf <- openFile "base.txt" WriteMode

  -- hPrint outf $ show ls
  -- hPrint outf $ show lsHex
  -- hPrint outf $ show lsBin

  myHPrint outf ls lsHex lsBin

  hClose outf

myHPrint outf [] _ _ = return ()
myHPrint outf (k1 : ve1) (k2 : ve2) (k3 : ve3) = do
  hPutStrLn outf $ show k1 ++ " -- " ++ k2 ++ " -- " ++ k3
  myHPrint outf ve1 ve2 ve3

mainFilmek = do
  inf <- openFile "filmek.txt" ReadMode
  temp <- hGetContents inf
  -- print temp
  let lsFilm = map fgMap $ lines temp
  print lsFilm
  hClose inf

fgMap str = (t1, t2, t3, t4)
  where
    temp = words str
    t1 = temp !! 0
    t2 = read (temp !! 1) :: Int
    t3 = temp !! 2 
    t4 = read (temp !! 3) :: Int