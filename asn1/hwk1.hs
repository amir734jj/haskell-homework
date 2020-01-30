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

dft_resolve_nested :: [((Double, Double), Double)] -> Double -> Int -> [(Double, Double)]
dft_resolve_nested [] _ _ = []
dft_resolve_nested (((x, y), t) : xs) k n = do
  let angle = 2.0 * pi * ( t) * ( k) / (fromIntegral n)
  let sumreal = x * (cos angle) + y * (sin angle)
  let sumimag = - x * (sin angle) + y * (cos angle)
  (sumreal, sumimag) : (dft_resolve_nested xs k n)


tuples_sum :: [(Double, Double)] -> (Double, Double)
tuples_sum [] = (0, 0)
tuples_sum ((x1, y1) : xs) = do
  let (x2, y2) = tuples_sum xs
  (x1 + x2, y1 + y2)


dft_resolve ::  [((Double, Double), Double)] -> [(Double, Double)]
dft_resolve [] = []
dft_resolve ls = do
  let n = ownLength ls
  let (((x, y), k) : xs) = ls
  let (xr, yr) = tuples_sum (dft_resolve_nested ls k n)
  (xr, yr) : (dft_resolve xs)


dft :: [(Double, Double)] -> [(Double, Double)]
dft [] = []
dft ls = dft_resolve (zip ls [0..])


-- Main driver
main = do
  print (range 0 1 10)
  print (rd 2 [2.123,3.456,4.675])
  print (absolute [(1,2), (3,4)] )
  print (dft [(1,2), (3,4)])