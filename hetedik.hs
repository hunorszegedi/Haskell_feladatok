-- lucky numbers

szita (k : ve) p = k : szita [m | (i, m) <- zip [p ..] ve, i `mod` k > 0] (p + 1)

foSzita n = take n $ 1 : szita [3, 5 ..] 3