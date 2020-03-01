import Control.Monad.Reader
import Data.Complex
import GHC.Base hiding (foldr)

newtype Vec a = Vec {runVec :: [a]}

-- ***************************
-- 1. Type alias Signal

-- ***************************

type Signal = Vec (Complex Double)

-- ***************************
-- 2. Type class instances

-- ***************************

instance Show a => Show (Vec a) where
  show (Vec lst) = "[" ++ drop 1 lst' ++ "]"
    where
      lst' = mconcat $ map (\x -> " " ++ show x) lst

instance Functor Vec where
  fmap f (Vec a) = Vec $ map f a

instance Applicative Vec where
  pure x = Vec (repeat x)
  (Vec f) <*> (Vec x) = Vec $ map (uncurry ($)) $ zip f x
  liftA2 f (Vec x) (Vec y) = Vec (zipWith f x y)

instance Num a => Num (Vec a) where
  x + y = liftA2 (+) x y
  x - y = liftA2 (-) x y
  x * y = liftA2 (*) x y
  negate x = fmap negate x
  abs x = fmap abs x
  signum x = fmap signum x
  fromInteger i = pure (fromInteger (i))

instance Fractional a => Fractional (Vec a) where
  x / y = liftA2 (/) x y
  fromRational x = pure (fromRational (x))

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

instance Foldable Vec where
  foldr f c (Vec a) = foldr f c a

imagV :: Num a => Vec a -> Vec (Complex a)
imagV (Vec a) = Vec $ map (0 :+) a

realV :: Num a => Vec a -> Vec (Complex a)
realV (Vec a) = Vec $ map (:+ 0) a

instance Semigroup (Vec a) where
  (<>) (Vec x) (Vec y) = Vec (x ++ y)

instance Monoid (Vec a) where
  mempty = Vec []

-- ***************************
-- 3. Auxiliary functions

-- ***************************

range :: Double -> Double -> Double -> [Double]
range from to 0 = []
range from to count = from : (range (from + (to - from) / count) to (count -1))

absolute :: Vec (Complex Double) -> Vec Double
absolute x = fmap magnitude x

-- auxiliary function rr n x that rounds the number x to n digits
rr :: Int -> Double -> Double
rr n x = (fromIntegral (round (x * (10 ^ n)))) / (10 ^ n)

rd :: Int -> Vec Double -> Vec Double
rd n x = fmap (rr n) x

length' :: (Vec a) -> Int
length' (Vec x) = length x

-- ***************************
-- 4. Implementation of twiddle, dft, and idft

-- ***************************

-- twiddle function
twiddle :: Double -> Double -> Signal
twiddle n k = Vec (map (\l -> cis (-2 * pi * k * l / n)) [0 .. (n -1)])

-- dft function
dft :: Signal -> Signal
dft list =
  let elist = map (\k -> runVec $ twiddle (fromIntegral $ length' list) (fromIntegral k)) [0 .. ((length' list) -1)]
      product = map (zipWith (*) (runVec list)) elist
   in Vec (map sum product)

-- idft function
idft :: Signal -> Signal
idft list =
  let cdft = dft $ fmap conjugate list
   in fmap (\y -> (conjugate y) / fromIntegral (length' list)) cdft

-- ***************************
-- 5. Low pass filter

-- ***************************

mask :: Int -> Int -> Signal
mask freq n = realV $ Vec $ one ++ zero ++ one
  where
    one = (take freq $ repeat 1)
    zero = (take (n - freq * 2) $ repeat 0)

-- low pass function
low_pass' :: Int -> Signal -> Signal
low_pass' freq list = idft $ liftA2 (*) (mask freq (length' list)) (dft list)

-- ***************************
-- 6. Efficient version of fft

-- ***************************

-- evenlist function that removes the elements in odd indexed elements
evenlist :: [a] -> [a]
evenlist [] = []
evenlist [x] = [x]
evenlist (x : xs) = x : (evenlist (tail (xs)))

-- oddlist function that removes the elements in even indexed elements
oddlist :: [a] -> [a]
oddlist [] = []
oddlist (x : xs) = evenlist (xs)

-- evenlist function for Signal
evenls :: Signal -> Signal
evenls list = Vec $ evenlist (runVec list)

-- oddlist function for Signal
oddls :: Signal -> Signal
oddls list = Vec $ oddlist (runVec list)

-- fft function
fft :: Signal -> Reader Signal Signal
fft list
  | length' list == 1 = return list
  | (length list) <= 16 = return $ dft list
  | otherwise = do
    twiddlelist <- ask
    let eventwiddlelist = evenls twiddlelist
    let elist = (runReader $ fft $ evenls list) eventwiddlelist
    let olist = (runReader $ fft $ oddls list) eventwiddlelist
    let product = liftA2 (*) twiddlelist olist
    return $ (<>) (liftA2 (+) elist product) (liftA2 (-) elist product)

-- ifft function
ifft :: Signal -> Reader Signal Signal
ifft list = do
  twiddlelist <- ask
  let cfft = (runReader $ fft $ fmap conjugate list) twiddlelist
  return $ fmap (\y -> (conjugate y) / fromIntegral (length' list)) cfft

-- low pass function
low_pass :: Int -> Signal -> Signal
low_pass freq list = (runReader $ ifft $ liftA2 (*) (mask freq (length' list)) ((runReader $ fft list) twiddlelist)) twiddlelist
  where
    twiddlelist = twiddle (fromIntegral $ length' list) 1

-- main for HW5
main = do
  let n = fromIntegral 2 ^ 8
  let s1 = fmap (\x -> sin (20 * pi * x) + sin (40 * pi * x) / 2) $ Vec $ range 0 1 n
  print (rd 3 s1)
  print (rd 3 $ fmap (\(r :+ _) -> r) $ low_pass' 15 $ realV s1)
  print (rd 3 $ fmap (\(r :+ _) -> r) $ low_pass 15 $ realV s1)