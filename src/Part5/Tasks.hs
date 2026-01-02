module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ res [] = res
myFoldl f res (x:xs) = myFoldl f (f res x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ res [] = res
myFoldr f res (x:xs) = f x $ myFoldr f res xs 

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f arr = myFoldl (\x -> \y -> x ++ [f y]) [] arr

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldl (\x -> \y -> x ++ (f y)) []

myConcat :: [[a]] -> [a]
myConcat = myFoldl (\x -> \y -> x ++ y) [] 

myReverse :: [a] -> [a]
myReverse arr = myFoldr (\x -> \y -> y ++ [x]) [] arr

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\x -> \y -> (myFilterHelper p x) ++ y) []

myFilterHelper :: (a -> Bool) -> a -> [a]
myFilterHelper p x = case p x of
    True -> [x]
    False -> []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\x -> \y -> partitionMerge (myPartitionHelper p x) y) ([], [])

myPartitionHelper :: (a -> Bool) -> a -> ([a], [a])
myPartitionHelper p x = case p x of
    True -> ([x], [])
    False -> ([], [x])

partitionMerge :: ([a], [a]) -> ([a], [a]) -> ([a], [a])
partitionMerge (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

