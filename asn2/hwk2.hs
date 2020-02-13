-- Name:       Seyedamirhossein Hesamian
-- ePantherID: hesamian
-- Notes:      Runs successfully and output matches with the expected output
import Data.Time


-- ********
-- HW #1: copy/paste from the provided solution
-- ********

range :: (Ord a, Fractional a) => a -> a -> a -> [a]
range from to count = next from count
  where
    step = (to - from) / count
    next from count
      | count <= 0 = []
      | otherwise = from : next (from + step) (count -1)

absolute :: Floating a => [(a, a)] -> [a]
absolute [] = []
absolute ((r, i) : rest) = sqrt (r * r + i * i) : absolute (rest)

rd :: (Integral b, RealFrac a) => b -> [a] -> [a]
rd _ [] = []
rd n (a : b) = f a : rd n b
  where
    f x = fromIntegral (round (c * x)) / c
    c = 10 ^ n

dft :: (Ord b, Floating b) => [b] -> [(b, b)]
dft x =
  let n = fromIntegral $ length x
      index = range 0 n n
      xn = x `zip` index
      f [] = []
      f (k : rest) = (sum r, sum i) : f rest
        where
          (r, i) = unzip $ factor xn
          factor [] = []
          factor ((xi, j) : rest) = (xi * cos y, - xi * sin y) : factor rest
            where
              y = 2 * pi * j * k / n
   in f index

-- ********
-- HW #2
-- ********
is_even n = mod n 2 == 0
is_odd n = not $ is_even n

-- split the list into two lists
split :: [a] -> ([a], [a])
split [] = ([], [])
split ls = split_handler ([], []) (zip ls [0 ..])
  where
    split_handler :: ([a], [a]) -> [(a, Int)] -> ([a], [a])
    split_handler (even, odd) [] = (even, odd)
    split_handler (even, odd) ((x, i) : xs) =
      if is_even i
        then split_handler (even ++ [x], odd) xs
        else split_handler (even, odd ++ [x]) xs

-- FFT DEFINITION with auxiliary functions

-- firsty calculates the first half of Y, that is Y_k for 0<= k < N/2
-- parameters of firsty are: the E list, the O list and the list of complex numbers e^(-2*pi*i*k/N), in that order
-- when firsty is called all its parameters have the same length, I assume this in order to define firsty
firsty :: [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
firsty [] _ _ = []
firsty (el : els) (ol : ols) (expl : expls) =
  let x = fst (el) + fst (expl) * fst (ol) - snd (expl) * snd (ol)
      y = snd (el) + fst (expl) * snd (ol) + snd (expl) * fst (ol)
   in (x, y) : (firsty els ols expls)

-- secondy calculates the second half of Y, that is Y_(k+N/2) for 0<= k < N/2
-- parameters of secondy are: the E list, the O list and the list of complex numbers e^(-2*pi*i*k/N), in that order
-- when secondy is called all its parameters have the same length, I assume this in order to define secondy
secondy :: [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
secondy [] _ _ = []
secondy (el : els) (ol : ols) (expl : expls) =
  let x = fst (el) - fst (expl) * fst (ol) + snd (expl) * snd (ol)
      y = snd (el) - fst (expl) * snd (ol) - snd (expl) * fst (ol)
   in (x, y) : (secondy els ols expls)

-- fft function
fft :: [Double] -> [(Double, Double)]
fft list
  | (length list) <= 16 = dft list
  | otherwise =
    let (evenlist, oddlist) = split list
        n = length list
        n_2 = n `div` 2
        elist = take n_2 (fft evenlist)
        olist = take n_2 (fft oddlist)
        explist = map (\l -> (cos (-2 * pi * fromIntegral (l) / fromIntegral (n)), sin (-2 * pi * fromIntegral (l) / fromIntegral (n)))) [0 .. (n_2 -1)]
     in (firsty elist olist explist) ++ (secondy elist olist explist)

-- explist is the list of complex numbers e^(-2*pi*i*k/N)
-- *** minus sign is included in both sin and cos (but not necessary in cos since cos(x)=cos(-x))

-- ********
-- MAIN
-- ********
main = do
  let n = 2 ^ 8
  let s1 = map (\x -> sin (20 * pi * x) + sin (40 * pi * x) / 2) $ range 0 1 n
  -- print(rd 3 s1)

  start <- getCurrentTime
  let dft1 = map (\x -> x /  (n)) $ absolute $ dft s1
  print (rd 2 dft1)
  end <- getCurrentTime
  print (diffUTCTime end start)
  start2 <- getCurrentTime
  let fft1 = map (\x -> x /  (n)) $ absolute $ fft s1
  print (rd 2 fft1)
  end2 <- getCurrentTime
  print (diffUTCTime end2 start2)