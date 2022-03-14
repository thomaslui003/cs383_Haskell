-- Student number: 
-- Student name: Thomas Lui
-- 383 H1

seqMultiple :: Integral a => a -> a -> [Bool]
seqMultiple n b = [  (i `mod` b) == 0 | i <- [1..n] ]

fib :: (Eq a, Num a, Num result) => a -> result
fib n
    |   n==0      = 0 
    |   n==1      = 1
    |   otherwise  = fib(n-1) + fib(n-2)

listReverse :: [a] -> [a]
listReverse [] = []
listReverse (a:rest) = listReverse rest ++ [a]

--recursive til one of the list is empty and the other have element, put the rest in the final list
listAdd :: Num a => [a] -> [a] -> [a]
listAdd [] ys = ys
listAdd  xs [] = xs
listAdd (x:xs) (y:ys) = x + y : listAdd xs ys

inList :: Eq t => [t] -> t -> Bool
inList [] _ = False 
inList (a:as) b 
 | a == b = True
 | otherwise = inList as b 

