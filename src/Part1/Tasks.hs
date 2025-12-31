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
myGCD = notImplementedYet

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect = notImplementedYet

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n power = if power < 0 then 0 else myPowHelper n power 1

myPowHelper :: Integer -> Integer -> Integer -> Integer
myPowHelper _ 0 res = res
myPowHelper n power res = myPowHelper n (power - 1) (res * n)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = notImplementedYet

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
