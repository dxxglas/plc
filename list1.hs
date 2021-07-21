{- q1 -}
potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n k = n * potencia n (k-1)

{- q2 -}
numDiv :: Integral a => a -> a -> a
numDiv x y
            | x `mod` y == 0    = 1 + numDiv (x `div` y) y
            | otherwise         = 0

{- q3 -}
unicos :: [Int] -> [Int]
unicos [] = []
unicos x
        | (head x) `elem` (tail x)   = unicos (removeElem (head x) x)
        | otherwise                  = (head x) : unicos (tail x)

removeElem :: Int -> [Int] -> [Int]
removeElem _ [] = []
removeElem x y 
                | x == (head y) = removeElem x (tail y)
                | otherwise     = (head y) : removeElem x (tail y)

{- q4 -}
remDiv :: Int -> [a] -> ([a],[a])
remDiv n x = ((take (n-1) x), (drop n x))

{- q5 -}
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge a []  = a
merge [] b  = b
merge a b
            | (head a) <= (head b)  = (head a) : merge (tail a) b
            | otherwise             = (head b) : merge (tail b) a

{- q6 -}
metade :: [a] -> ([a],[a])
metade x = ((reverse(par x)), (reverse(impar x)))

par :: [a] -> [a]
par [] = []
par x      
        | (length x) `mod` 2 /= 0 = (last x) : (par (init x))
        | otherwise = par (init x)

impar :: [a] -> [a]
impar [] = []
impar x      
        | (length x) `mod` 2 == 0 = (last x) : (impar (init x))
        | otherwise = impar (init x)

{- q7 -}
msort :: Ord a => [a] -> [a]
msort []    = []
msort [x]   = [x]
msort x = merge (msort (take ((length x) `div` 2) x)) (msort (drop ((length x) `div` 2) x))