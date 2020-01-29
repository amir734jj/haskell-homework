
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
rd n x = map (\r -> truncate(r * (tens n)) `div` (tens n)) x

main = do
  print (rd 2 [2.123,3.456,4.675])

main = do
  print (range 0 1 10)
