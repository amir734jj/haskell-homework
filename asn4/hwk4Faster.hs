import Data.Complex
import Data.Time
import GHC.Base

newtype Vec a = Vec {runVec :: [a]}

-- implementation of Show in order to print vectors
instance Show a => Show (Vec a) where
   show (Vec x) = show x


-- ***************************
-- 1. You should implement the instances of Vec for the following type classes.
-- ***************************
-- (a) Functor class, where you should implement the fmap function.

instance Functor Vec where
  fmap f (Vec a) = Vec (map f a)

-- b) Applicative class, where you should implement the pure and liftA2
-- functions. (You should import it with import GHC.Base (liftA2).)

instance Applicative Vec where
  pure x = Vec (repeat x)
  liftA2 f (Vec x) (Vec y) = Vec (zipWith f x y)

-- (c) Semigroup class, where you should implement the <> operator.

instance Semigroup (Vec a) where
  (<>) (Vec x) (Vec y) = Vec (x ++ y)

-- (d) Monoid class, where you should implement the mempty function.

instance Monoid (Vec a) where
  mempty = Vec []


-- ***************************
-- 2. You should simplify the Vec instances of Num, Fractional, and Floating
-- using fmap, pure, and liftA2 as appropriate.
-- ***************************
-- Num class, where you should implement the operators (+), (-), (*) and
-- the functions negate, abs, signum, fromInteger. All but fromInteger
-- perform elementwise operations on their operands while fromInteger x
-- converts an integer x to a vector with infinite number of (fromInteger x).

instance Num a => Num (Vec a) where
   x + y = liftA2 (+) x y
   x - y = liftA2 (-) x y
   x * y = liftA2 (*) x y
   negate x = fmap negate x
   abs x = fmap abs x
   signum x = fmap signum x
   fromInteger i = pure (fromInteger(i))

instance Fractional a => Fractional (Vec a) where
  x / y = liftA2 (/) x y
  fromRational x = pure (fromRational(x))

instance Floating a => Floating (Vec a) where
  pi = pure pi
  exp x = fmap exp x
  log x = fmap log x
  sin x = fmap sin x
  cos x = fmap cos x
  asin x = fmap asin x
  acos x = fmap acos x
  atan x = fmap atan x
  sinh x = fmap sinh x
  cosh x = fmap cosh x
  asinh x = fmap asinh x
  acosh x = fmap acosh x
  atanh x = fmap atanh x


-- ***************************
-- 3. You should simplify the implementation of dft and fft functions to leverge
-- Vec Double and Vec (Complex Double). The final implementation should be closer
-- to the mathematical representation of the algorithms.
-- The functions should be given the following types:
-- range :: Double -> Double -> Double -> [Double]
-- absolute :: Vec (Complex Double) -> Vec Double
-- rd :: Int -> Vec Double -> Vec Double
-- dft :: [Double] -> Vec (Complex Double)
-- fft :: [Double] -> Vec (Complex Double)
-- ***************************

range :: Double -> Double -> Double -> [Double]
range from to 0 = []
range from to count = from:(range (from+(to-from)/count) to (count-1))

absolute :: Vec (Complex Double) -> Vec Double
absolute x = fmap magnitude x

-- auxiliar function rr n x that rounds the number x to n digits
rr :: Int -> Double -> Double
rr n x = (fromIntegral(round (x*(10^n)))) / (10^n)

rd :: Int -> Vec Double -> Vec Double
rd n x = fmap (rr n) x


-- DTF DEFINITION with auxiliary functions
-- This is a new definition and uses Complex

-- expklist is the list of complex numbers e^(-2*pi*k*n*i/r) for a given k and 0 <= n < r
expklist :: Int -> Int -> [Complex Double]
expklist k r = map (\l -> cis(-2*pi*fromIntegral(k)*fromIntegral(l)/fromIntegral(r))) [0..(r-1)]

-- dft function
dft :: [Double] -> Vec (Complex Double)
dft list =
  let elist = map (\k -> expklist k (length list)) [0..((length list)-1)]
      clist = map (\l -> l :+ 0) list
      product = map (zipWith (*) clist) elist
  in Vec (map sum product)


-- FFT DEFINITION with auxiliary functions

-- evenlist function that removes the elements in odd indexed elements
evenlist :: [a] -> [a]
evenlist [] = []
evenlist [x] = [x]
evenlist (x:xs) = x:(evenlist(tail(xs)))

-- oddlist function that removes the elements in even indexed elements
oddlist :: [a] -> [a]
oddlist [] = []
oddlist (x:xs) = evenlist(xs)

-- split function
split :: [a] -> ([a],[a])
split xs = (evenlist(xs),oddlist(xs))

-- fft function
fft :: [Double] -> Vec (Complex Double)
fft list
  | (length list) <= 16 = dft list
  | otherwise =
      let (evenlist,oddlist) = split list
          n = length list
          n2 = n `div` 2
          elist = fft evenlist
          olist = fft oddlist
          explist = fmap (\l -> cis(-2*pi*fromIntegral(l)/fromIntegral(n))) (Vec [0..(n2-1)])
          product = liftA2 (*) explist olist
      in (<>) (liftA2 (+) elist product) (liftA2 (-) elist product)

-- explist is the list of complex numbers e^(-2*pi*i*k/N), 0 <= k < N/2
-- we cannot use expklist to get explist since the denominator n in explist is not equal to n2


-- main for HW4
main = do
         let n = 2^8
         let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n

         start <- getCurrentTime
         let dft1 = fmap (/n) $ absolute $ dft s1
         print(rd 2 dft1)
         end <- getCurrentTime
         print (diffUTCTime end start)

         start2 <- getCurrentTime
         let fft1 = fmap (/n) $ absolute $ fft s1
         print(rd 2 fft1)
         end2 <- getCurrentTime
         print (diffUTCTime end2 start2)
