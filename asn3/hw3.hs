data Vec a = Vec [a]

instance Show a => Show (Vec a) where
   show t = showTree a

main = do
    let v1 = Vec [1,2,3]
    let v2 = Vec [2,3,4]
    let v3 = Vec [-10,0,10]
    printf v1
    printf v2
    printf v3
