{- q1 -}
somaSqrt :: [Double] -> Double
somaSqrt = foldr (+) 0 . map(sqrt) . filter (>0)

{- q2 -}
perfeitos :: Integer -> [Integer]
perfeitos a = reverse (filter (\x -> x == (sum . unicosAoQuadrado)(fatores x)) [2 .. a]) ++ [1]

fatores :: Integer -> [Integer]
fatores n 
    | fator == 0 = [n]
    | otherwise = fator : fatores(n `div` fator)
    where fator = menorDivisor n
    
menorDivisor :: Integer -> Integer 
menorDivisor n
        | null divisores = 0
        | otherwise = head divisores
        where divisores = filter (\x -> (n `mod` x) == 0) [2 .. n-1]

unicosAoQuadrado :: [Integer] -> [Integer]
unicosAoQuadrado [] = []
unicosAoQuadrado (x:xs)
        | x `elem` xs = unicosAoQuadrado xs
        | otherwise   = x*x : unicosAoQuadrado xs

{- q3 -}
unzip':: [(a,b)] -> ([a],[b])
unzip' = foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([], [])

{- ... -}
type Texto = String
type Id = String
type DataHoraPub = Int

data Post = Post (Id , DataHoraPub) Texto deriving (Show , Eq, Read)
data Thread = Nil | T Post (Thread) deriving (Read)

{- q4 -}
instance Show Thread where
    show Nil = ""
    show (T (Post (a, b) c) d) = "(" ++ a ++ " " ++ show b ++ " " ++ c  ++ ")" ++ show d

{- q5 -}
inserirPost :: Post -> Thread -> Thread
inserirPost (Post(a, b) c) Nil = (T (Post (a,b) c) Nil)
inserirPost (Post(a, b) c) (T (Post (d, e) f) g) = (T (Post (a, b) c) (T (Post (d,e) f) g))

{- q6 -}
threadToList :: Thread -> [Post]
threadToList Nil = []
threadToList (T (Post (a,b) c) Nil) = [(Post(a, b) c)]
threadToList (T (Post (a,b) c) d) = [(Post(a, b) c)] ++ threadToList d

{- q7 -}
listToThread :: [Post] -> Thread
listToThread [] = Nil
listToThread (a:as) = T (a) (listToThread as)

{- q8 -}
removerPost :: (Id, DataHoraPub) -> Thread -> Thread
removerPost a Nil = Nil
removerPost a b = listToThread (filter(\b-> idData b /= a) (threadToList b))

idData :: Post -> (Id, DataHoraPub)
idData (Post(a, b) c) = (a,b)