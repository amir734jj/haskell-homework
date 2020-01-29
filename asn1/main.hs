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

-- Main driver
main = do
  print (range 0 1 10)
  print (rd 2 [2.123,3.456,4.675])
  print (absolute [(1,2), (3,4)] )
