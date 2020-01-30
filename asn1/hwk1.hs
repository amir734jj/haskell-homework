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


-- void dft(float x[], float result[], uint32_t num_elems) {
--   // See: "modified C code" from https://batchloaf.wordpress.com/2013/12/07/simple-dft-in-c/ 
--   // to simplify does not use pre-computed cos/sin(z)
--   for(uint32_t k = 0; k < num_elems; k++) {
--     float xre[num_elems]; // Real component
--     float xim[num_elems]; // Imaginary component
--     for(uint64_t n = 0; n < num_elems; n++) {
--       float z = (2 * M_PI * k * n) / num_elems;
--       xre[n] += x[n] * cos(z);
--       xim[n] -= x[n] * sin(z);
--     }
--     result[k] = (xre[k] * xre[k]) * (xim[k] * xim[k]);
--   }
-- }

-- Length of the array
ownLength :: [t] -> Int
ownLength [] = 0
ownLength (_: xs) = 1 + ownLength xs

dft_resolve_nested :: [(Double, Double)] -> Double -> Double -> Int -> [(Double, Double)]
dft_resolve_nested [] _ _ _ = []
dft_resolve_nested ((x, k) : xs) xk k num_elems = do
  let angle = 2.0 * pi * t * k / (fromIntegral n)
  let sumreal = x * (cos angle) + y * (sin angle)
  let sumimag = - x * (sin angle) + y * (cos angle)
  (sumreal, sumimag) : (dft_resolve_nested xs k n)


tuples_sum :: [(Double, Double)] -> (Double, Double)
tuples_sum [] = (0, 0)
tuples_sum ((x1, y1) : xs) = do
  let (x2, y2) = tuples_sum xs
  (x1 + x2, y1 + y2)


dft_resolve ::  [(Double, Double)] -> [(Double, Double)]
dft_resolve [] = []
dft_resolve ls = do
  let num_elems = ownLength ls
  let ((x, k) : xs) = ls
  let (xr, yr) = tuples_sum (dft_resolve_nested ls x k num_elems)
  (xr, yr) : (dft_resolve xs)


-- Entry point to calculate the DFT
dft :: [Double] -> [(Double, Double)]
dft [] = []
dft ls = dft_resolve (zip ls [0..])


-- Main driver
main = do
  let n = 64
  let s = map (\t -> sin(10*2*pi*t) + sin(20*2*pi*t)/2) $ range 0 1 n
  let result = s
  -- print(rd 3 s)
  -- print(rd 2 result)
  print result