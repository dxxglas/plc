ehZero :: Int -> Bool
ehZero 0 = True
ehZero n = False

potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n k = n * potencia n (k-1)

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour m n p q
                | (m >= maxThree n p q) = m
                | (maxThree n p q >= m) = maxThree n p q

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z 
                | x >= y && x >= z = x
                | y >= x && y >= z = y
                | z >= x && z >= y = z

maxFour' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour' m n p q 
                | (max m n) >= (max p q) = max m n
                | otherwise = max p q

maxFour'' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour'' m n p q
                    | max m n >= maxThree n p q = max m n
                    | otherwise = maxThree n p q

binomial :: Int -> Int -> Int
binomial 0 k 
            | k > 0 = 0
binomial n 0 = 1
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

quadruplo :: Integer -> Integer
quadruplo x = dobro (dobro x)

dobro :: Integer -> Integer
dobro n = n * 2

poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a * x^2 + b * x + c

sumTo :: Int -> Int
sumTo n
        | n == 1 = 1
        | otherwise = n + sumTo(n - 1)

parImpar :: Int -> String
parImpar n 
            | n `mod` 2 == 0 = "par"
            | otherwise = "impar"

paraDireita :: Int -> String -> String
paraDireita n s = s ++ addEspacos(n)

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos(n-1)

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual m n p
                    | m == n && m == p = 3
                    | m == n || m == p || n == p = 2
                    | otherwise = 0

tribonacci :: Int -> Int
tribonacci 1 = 1 
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci (n-1) + tribonacci(n-2) + tribonacci(n-3)