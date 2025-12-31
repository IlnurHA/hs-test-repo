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
mostFreq = charToInt . getMax . stringify

stringify :: [Int] -> String
stringify [] = ""
stringify (x:xs) = show x ++ stringify xs

-- get max count for digits
getMax :: String -> Char
getMax string = checkFor string '0' (-1) '-'

getFor :: String -> Char -> Int
getFor arr x = length $ filter (==x) arr

checkFor :: String -> Char -> Int -> Char -> Char
checkFor string char@'9' max_ maxchar
    | max_ > getFor string char = maxchar
    | otherwise = char
checkFor string char max_ maxchar = checkFor string (getNext char) nextMax nextMaxChar
    where
        nextMax = max max_ $ getFor string char
        nextMaxChar = if nextMax == max_ then maxchar else char 

getNext :: Char -> Char
getNext '0' = '1'
getNext '1' = '2'
getNext '2' = '3'
getNext '3' = '4'
getNext '4' = '5'
getNext '5' = '6'
getNext '6' = '7'
getNext '7' = '8'
getNext '8' = '9'
getNext _ = '0'

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt _ = -1

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
grokBy f l = notImplementedYet
