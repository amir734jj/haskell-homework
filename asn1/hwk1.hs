-- Creates a Double array given (from, to, count)
range :: Integer -> Integer -> Integer -> [Double]
range from to 0 = [];
range from to count = do
  let increment = fromIntegral(to - from) / fromIntegral count
  let lst = [0] ++ [increment, increment + increment ..]
  take (fromIntegral count) lst

-- Create 10^n
tens :: Int -> Integer
tens n = fromIntegral(product (take n [10, 10 .. ]))

-- Rounds list of numbers to n decimal places
rd :: Int -> [Double] -> [Double]
rd n x = map (\r -> fromIntegral(truncate(r * fromIntegral(tens n))) / fromIntegral(tens n)) x

-- Creates a Double array given (from, to, count)
absolute :: [(Double, Double)] -> [Double]
absolute [] = []
absolute ((x,y) : xs) = (sqrt (x^2 + y^2)) : (absolute xs)


-- void dft(double[] inreal , double[] inimag, double[] outreal, double[] outimag) {
--     int n = inreal.length;
--     for (int k = 0; k < n; k++) {      // For each output element
--         double sumreal = 0;
--         double sumimag = 0;
--         for (int t = 0; t < n; t++) {  // For each input element
--             double angle = 2 * Math.PI * t * k / n;
--             sumreal +=  inreal[t] * Math.cos(angle) + inimag[t] * Math.sin(angle);
--             sumimag += -inreal[t] * Math.sin(angle) + inimag[t] * Math.cos(angle);
--         }
--         outreal[k] = sumreal;
--         outimag[k] = sumimag;
--     }
-- }

-- Length of the array
ownLength :: [t] -> Int
ownLength [] = 0
ownLength (_: xs) = 1 + ownLength xs

-- 
dft_resolve :: [((Int, Int), Int)] -> Int
dft_resolve (((x, y), i) : xs) = x

-- 
dft :: [t] -> [(t, Int)]
dft [] = []
dft ls = (zip ls [0..])

-- Main driver
main = do
  print dft_resolve([((1,2), 3)])
  print (dft [(1,2), (3,4)])

-- Main driver
main = do
  print (range 0 1 10)
  print (rd 2 [2.123,3.456,4.675])
  print (absolute [(1,2), (3,4)] )
