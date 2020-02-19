-- Name:       Seyedamirhossein Hesamian
-- ePantherID: hesamian
-- Notes:      Runs successfully and output matches with the expected output
import Data.Complex
import Prelude hiding (pure)

data Vec a = Vec [a]

-- Show class, where you should implement the show function.
-- The show function should print Vec [1,2,3] as [1 2 3].
instance Show a => Show (Vec a) where
   show (Vec x)   = show x

-- Custom vector operation
vec_op1 :: (a -> a) -> [a] -> Vec a
vec_op1 op x = Vec (map (\r -> op r) x)

vec_op2 :: (a -> a -> a) -> [a] -> [a] -> Vec a
vec_op2 op x y = Vec (map (\r -> (fst r) `op` (snd r)) $ zip x y)

-- Num class, where you should implement the operators (+), (-), (*) and
-- the functions negate, abs, signum, fromInteger. All but fromInteger
-- perform elementwise operations on their operands while fromInteger x
-- converts an integer x to a vector with infinite number of (fromInteger x).
instance Num a => Num (Vec a) where
   fromInteger i     = Vec (cycle [fromInteger(i)])
   signum (Vec x)    = vec_op1 (signum) x
   abs  (Vec x)      = vec_op1 (abs) x
   negate  (Vec x)   = vec_op2 (*) x (cycle [-1])
   (Vec x) + (Vec y) = vec_op2 (+) x y
   (Vec x) * (Vec y) = vec_op2 (*) x y

-- Fractional class, where you should implement (/), fromRational functions.
-- The division operator is also elementwise while fromRational x converts x to
-- a vector with infinite number of (fromRational x).
instance Fractional a => Fractional (Vec a) where
  (Vec x) / (Vec y) = Vec (zipWith (/) x y)
  fromRational x = Vec (repeat $ fromRational(x))

-- Floating class, where you should implement pi, exp, log, sin, cos, asin, acos, atan, sinh,
-- cosh, asinh, acosh, atanh functions. pi is a vector of infinite pi while other functions
-- are elementwise operations on their operands.
instance Floating a => Floating (Vec a) where
  pi = Vec (repeat pi)
  exp (Vec x) = Vec (map exp x)
  log (Vec x) = Vec (map log x)
  sin (Vec x) = Vec (map sin x)
  cos (Vec x) = Vec (map cos x)
  asin (Vec x) = Vec (map asin x)
  acos (Vec x) = Vec (map acos x)
  atan (Vec x) = Vec (map atan x)
  sinh (Vec x) = Vec (map sinh x)
  cosh (Vec x) = Vec (map cosh x)
  asinh (Vec x) = Vec (map asinh x)
  acosh (Vec x) = Vec (map acosh x)
  atanh (Vec x) = Vec (map atanh x)

-- Foldable class, where you should implement foldr function, which folds the list in the Vec.
instance Foldable Vec where
  foldr f x (Vec y) = foldr f x y


-- pure, which converts a constant x to a Vec of infinite number of x.
-- I have to import Prelude hiding pure because pure is already defined in Prelude
-- we can change the name to, for example, pureV and then omit the part "import Prelude hiding (pure)" above
-- if you change the name then it has to be changed also in the main part below
pure :: a -> Vec a
pure x = Vec (repeat x)


-- realV, which converts a Vec of numbers into a Vec of complex numbers where the imaginary parts are 0.
realV :: Num a => Vec a -> Vec (Complex a)
realV (Vec y) = Vec (map (\x -> x :+ 0) y)

-- imagV, which converts a Vec of numbers into a Vec of complex numbers where the real parts are 0.
-- The constructor (:+) forms a complex number from its real and imaginary rectangular components.
-- Source: https://www.haskell.org/onlinereport/complex.html
imagV :: Num a => Vec a -> Vec (Complex a)
imagV (Vec y) = Vec (map (\x ->  0 :+ x) y)


-- Main part
main = do
   let v1 = Vec [1,2,3]
   let v2 = Vec [2,3,4]
   let v3 = Vec [-10,0,10]
   print $ v1 + v2
   print $ v1 - v2
   print $ v1 * v2
   print $ v1 / v2
   print $ negate v1
   print $ signum v3
   print $ abs v3
   print $ v1 + 10
   print $ v2 + 1.2

   print $ v1 + (pure $ sqrt 2)
   print $ realV v1
   print $ imagV v1
   print $ realV v1 + imagV v2
   print $ sin $ v1 * (pi / 2)
   print $ sum v1