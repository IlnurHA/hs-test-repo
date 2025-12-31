module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n) : (finc f (n + 1))

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : (ff f (f x))

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq = mostFreqHelper . stringify

stringify :: [Int] -> String
stringify [] = ""
stringify (x:xs) = show x ++ stringify xs

getFor :: String -> Char -> Int
getFor arr x = length $ filter (==x) arr

getForFromInt :: String -> Int -> Int
getForFromInt arr x = getFor arr (intToChar x)

mostFreqHelper :: String -> Int
mostFreqHelper string = getIndexOf (maxFromList (-1) $ convertedList) convertedList
    where
        convertedList = take 10 $ finc (getForFromInt string) 0

intToChar :: Int -> Char
intToChar 0 = '0'
intToChar 1 = '1'
intToChar 2 = '2'
intToChar 3 = '3'
intToChar 4 = '4'
intToChar 5 = '5'
intToChar 6 = '6'
intToChar 7 = '7'
intToChar 8 = '8'
intToChar 9 = '9'
intToChar _ = '-'

maxFromList :: (Ord a) => a -> [a] -> a
maxFromList res [] = res
maxFromList res (x:xs) = maxFromList (max res x) xs

getIndexOf :: (Ord a) => a -> [a] -> Int
getIndexOf x arr = getIndexOfHelper x arr 0

getIndexOfHelper :: (Ord a) => a -> [a] -> Int -> Int
getIndexOfHelper _ [] _ = -1
getIndexOfHelper n (x:xs) index | x == n = index
                                | otherwise = getIndexOfHelper n xs (index + 1)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = uniqHelper []

uniqHelper :: (Eq a) => [a] -> [a] -> [a]
uniqHelper res [] = res
uniqHelper res (x:xs) | isIn x res = uniqHelper res xs
                    | otherwise = uniqHelper (x:res) xs

isIn :: (Eq a) => a -> [a] -> Bool
isIn a [] = False
isIn a (x:xs) | x == a = True
            | otherwise = isIn a xs


-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = grokByHelper f l []

grokByHelper :: (Eq k) => (a -> k) -> [a] -> [(k, [a])] -> [(k, [a])]
grokByHelper f [] res = res
grokByHelper f (x:xs) res = grokByHelper f xs $ addResult res (f x) x

addResult :: (Eq k) => [(k, [a])] -> k -> a -> [(k, [a])]
addResult [] k a = [(k, [a])] 
addResult (x@(kDict, arr):xs) k a   | kDict == k = (k, a:arr):xs
                                    | otherwise = x : (addResult xs k a)
