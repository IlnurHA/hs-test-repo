module Part1.Tasks where

import Util(notImplementedYet)

factorial :: Int -> Int
factorial x = factorialHelper x 1

factorialHelper :: Int -> Int -> Int
factorialHelper x res = if x <= 1 then res else factorialHelper (x - 1) (res * x)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = (mySinHelper 2 7 x x)

mySinHelper :: Int -> Int -> Double -> Double -> Double 
mySinHelper i n x res = if i > n then res
    else (mySinHelper (i + 1) n x (res + nextSummand) )

    where
        sign = (-1.0) ** fromIntegral(i + 1)
        power = (2 * i - 1)
        fact = factorial power
        
        nominator :: Double
        nominator = sign * ( x ** (fromIntegral power) )
        
        nextSummand :: Double
        nextSummand = nominator / (fromIntegral fact)
        

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = myCosHelper 1 7 x 1

myCosHelper :: Int -> Int -> Double -> Double -> Double 
myCosHelper i n x res = if i > n then res
    else (myCosHelper (i + 1) n x (res + nextSummand) )

    where
        sign = (-1.0) ** fromIntegral(i)
        power = (2 * i)
        fact = fromIntegral $ factorial power
        
        nominator :: Double
        nominator = sign * ( x ** (fromIntegral power) )
        
        nextSummand :: Double
        nextSummand = nominator / fact

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD 0 b = abs b
myGCD a b = myGCDHelper (max a' b') (min a' b')
    where
        a' = abs a
        b' = abs b

myGCDHelper :: Integer -> Integer -> Integer
myGCDHelper gr ls = if gr `mod` ls == 0 then ls else myGCDHelper ls (gr `mod` ls)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
-- Assuming years should be non negative
-- Day, Month, Year
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y =
    if y < 0 then False else             -- min year check
    if (min d m) <= 0 then False else    -- min days/months check
    if m > 12 then False else            -- max months check
    if d > (maxDays m y) then False else -- max days check
    True

-- 31 days: 1, 3, 5, 7, 8, 10, 12
-- 30 days: 4, 6, 9, 11
-- 28 or 29: 2

-- Month -> Year -> Days
maxDays :: Integer -> Integer -> Integer
maxDays 2 year = if leapYear year then 29 else 28
maxDays x _ | x > 7 = if x `mod` 2 == 0 then 31 else 30
maxDays x _ = if x `mod` 2 == 0 then 30 else 31 

leapYear :: Integer -> Bool
leapYear year =
    if year `mod` 100 /= 0 && year `mod` 4 == 0 then True else
    if year `mod` 400 == 0 then True
    else False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n power = if power < 0 then 0 else myPowHelper n power 1

myPowHelper :: Integer -> Integer -> Integer -> Integer
myPowHelper _ 0 res = res
myPowHelper n power res = myPowHelper n (power - 1) (res * n)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = isPrimeHelper 2

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper _ 1 = False
isPrimeHelper cur x | cur == x = True
isPrimeHelper cur x = if x `mod` cur == 0 then False else isPrimeHelper (cur + 1) x

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = area
    where
        leftSum = case points of 
            [] -> 0
            ((a, b) : xs) -> gaussLeftSum points 0 b 
        

        rightSum = case points of 
            [] -> 0
            ((a, b) : xs) -> gaussRightSum points 0 a
        
        area = (abs(leftSum - rightSum)) / 2 

gaussLeftSum :: [Point2D] -> Double -> Double -> Double
gaussLeftSum [] x firstY = x * firstY
gaussLeftSum ((a, b):xs) x firstY = b * x + (gaussLeftSum xs a firstY)


gaussRightSum :: [Point2D] -> Double -> Double -> Double
gaussRightSum [] y firstX = y * firstX
gaussRightSum ((a, b):xs) y firstX = a * y + (gaussRightSum xs b firstX)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c =
    if not (notATriangleTest a b c) then -1 else
    computeTriangleTest

    where 
        max_side = max a $ max b c

        computeTriangleTest = case max_side of
            x | x == a -> triangleTest a b c
            x | x == b -> triangleTest b a c
            x -> triangleTest c a b
        triangleTest max_side side1 side2 = 
            if max_side * max_side > side1 * side1 + side2 * side2 then 0 else
            if max_side * max_side == side1 * side1 + side2 * side2 then 2 else
            1

notATriangleTest :: Double -> Double -> Double -> Bool
notATriangleTest a b c =
    if a + b < c then False else
    if a + c < b then False else
    if b + c < a then False else
    True

