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

main = do
   let v1 = Vec [1,2,3]
   let v2 = Vec [2,3,4]
   let v3 = Vec [-10,0,10]
   print v1
   print v2
   print v3
   print $ v1 + v2
   print $ negate v1
   print $ v1 * v2