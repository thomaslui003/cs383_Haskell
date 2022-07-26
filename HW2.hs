{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import GHC.Base (VecElem(Int16ElemRep))

-- Student number:
-- Student name: Thomas Lui
-- 383 H2


sumTailRec :: Num a => [a] -> a
sumTailRec k = sumTailAux k 0
 where
     sumTailAux [] b = b
     sumTailAux (x:xs) b = sumTailAux xs (b + x)

-- recursively apply the operator to the first element until empty list
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v [] = v
myFoldl f v (x:xs) = myFoldl f (f v x) xs

-- recursively apply the operator to the last element first until empty list
myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr f j [] = j
myFoldr f j (x:xs) = f x (myFoldr f j xs)


alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap f w [] = []
--if there's only one element apply only the f function to x and get the list
alternativeMap f w [x] = [f x]
--first two element and apply f to first element and w to the second one until list is empty
alternativeMap f w (x1 : x2 : xs) = f x1 : w x2 : alternativeMap f w xs


--if apply the f function (operator) on x element and get true then append the element into the empty acc list else append nothing to it 
-- foldl apply operation to all element in the original list
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldl (\acc x  -> if f x then acc ++ [x] else acc ++ [] ) [] xs

-- pointfree style of first taking all element and square it then filter all even integers and sum all of them up
sumsqeven  :: [Int] -> Int
sumsqeven  = sum . filter (even) . map (^2)




data List a = Empty | Cons { listHead :: a, listTail :: List a } deriving (Eq, Ord, Show, Read)
listConcat :: List a -> List a -> List a
listConcat Empty ys = ys
listConcat (Cons x xs) ys = Cons x (listConcat xs ys)

--listConcat  (Cons 1 Empty)  (Cons 2 Empty)
--Cons {listHead = 1, listTail = Cons {listHead = 2, listTail = Empty}}

