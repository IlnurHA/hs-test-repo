{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       mxzero :: Int -> Int -> mx
       -- Without values from provided coordinates
       submatrix :: mx -> Int -> Int -> mx
       size :: mx -> (Int, Int)
       row :: mx -> Int -> [Int]
       column :: mx -> Int -> [Int]
       insert :: mx -> (Int, Int) -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       mxzero _ _ = 0

       -- submatrix :: Int -> Int -> Int -> Int
       submatrix mx 0 0 = 0
       submatrix mx _ _ = mx
       
       -- size :: Int -> (Int, Int)
       size _ = (1, 1)

       -- row :: Int -> Int -> [Int]
       row mx 0 = [mx]
       row mx _ = []

       -- column :: Int -> Int -> [Int]
       column mx 0 = [mx]
       column mx _ = []

       insert _ (0, 0) value = value
       insert mx _ _ = mx

-- Length
myLength :: [a] -> Int
myLength = lengthHelper 0

lengthHelper :: Int -> [a] -> Int
lengthHelper res [] = res
lengthHelper res (_:xs) = lengthHelper (res + 1) xs

-- Exclude by index
exclude :: [a] -> Int -> [a]
exclude = excludeHelper 1

excludeHelper :: Int -> [a] -> Int -> [a]
excludeHelper _ [] _ = []
excludeHelper x0 arr@(x:xs) x1
              | x0 == x1 = xs
              | x1 > x0 = arr
              | otherwise = x : excludeHelper (x0 + 1) xs (x1)

-- Get by index
getByIndex :: a -> [a] -> Int -> a
getByIndex = getByIndexHelper 0

getByIndexHelper :: Int -> a -> [a] -> Int -> a
getByIndexHelper _ def [] _ = def
getByIndexHelper x0 def arr@(x:xs) x1
              | x0 == x1 = x
              | x1 > x0 = def
              | otherwise = getByIndexHelper (x0 + 1) def xs x1

instance Matrix [[Int]] where
       mxzero x y = mxzeroHelper x y []
              where
                     mxzeroHelper 0 _ res = res
                     mxzeroHelper x y res = mxzeroHelper (x - 1) y ((zerorow y []) : res)

                     zerorow 0 res = res
                     zerorow y res = zerorow (y - 1) (0:res)
       -- submatrix :: [[Int]] -> Int -> Int -> [[Int]]
       submatrix arr x y = Prelude.map (\subarr -> exclude subarr y) (exclude arr x)                     

       -- size :: [[Int]] -> (Int, Int)
       size [] = (0, 0)
       size arr@([]:xs) = (myLength arr, 0)
       size arr@(x:xs) = (myLength arr, myLength x)

       -- row :: [[Int]] -> Int -> [Int]
       row = getByIndex []

       -- column :: [[Int]] -> Int -> [Int]
       column arr y = Prelude.map (\x -> getByIndex 0 x y) arr

       -- insert :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
       insert mx (x, y) newValue = new_mx
              where
                     new_mx = insertHelper 0 mx x (\row -> insertHelper 0 row y (\_ -> newValue))

                     insertHelper :: Int -> [a] -> Int -> (a -> a) -> [a]
                     insertHelper _ [] _ _ = []
                     insertHelper index arr@(x:xs) targetIndex f
                            | index == targetIndex = (f x) : xs
                            | index > targetIndex = arr
                            | otherwise = x : (insertHelper (index + 1) xs targetIndex f)

instance Matrix (SparseMatrix Int) where
       mxzero x y = SparseMatrix { sparseMatrixWidth=y, sparseMatrixHeight=x, sparseMatrixElements=Data.Map.empty }

       -- submatrix :: SparseMatrix -> Int -> Int -> SparseMatrix
       submatrix mx x y = SparseMatrix { sparseMatrixWidth=yMax - 1, sparseMatrixHeight=xMax - 1, sparseMatrixElements=yMoved $ xMoved elems }
              where
                     elems :: Map (Int, Int) Int
                     elems = sparseMatrixElements mx 
                     xMax :: Int
                     xMax = sparseMatrixHeight mx
                     yMax :: Int
                     yMax = sparseMatrixWidth mx

                     xMoved :: Map (Int, Int) Int -> Map (Int, Int) Int
                     xMoved mx = moveValues x xMax yMax constructKeyY $ exclude constructKeyX x yMax mx
                     yMoved :: Map (Int, Int) Int -> Map (Int, Int) Int
                     yMoved mx = moveValues y yMax xMax constructKeyX $ exclude constructKeyY y xMax mx

                     constructKeyX :: Int -> Int -> (Int, Int)
                     constructKeyX x = \yIndex -> (x, yIndex)
                     constructKeyY :: Int -> Int -> (Int, Int)
                     constructKeyY y = \xIndex -> (xIndex, y)

                     exclude :: (Int -> Int -> (Int, Int)) -> Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
                     exclude = excludeHelper 0

                     excludeHelper :: Int -> (Int -> Int -> (Int, Int)) -> Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
                     excludeHelper index constructKey fixedIndex maxIndex mx 
                                   | index >= maxIndex = mx
                                   | otherwise = excludeHelper (index + 1) constructKey fixedIndex maxIndex (delete (constructKey fixedIndex index) mx)

                     moveValues :: Int -> Int -> Int -> (Int -> Int -> (Int, Int)) -> Map (Int, Int) Int -> Map (Int, Int) Int
                     moveValues = moveValuesHelper 0

                     moveValuesHelper :: Int -> Int -> Int -> Int -> (Int -> Int -> (Int, Int)) -> Map (Int, Int) Int -> Map (Int, Int) Int
                     moveValuesHelper curOtherIndex fromIndex toIndex maxOtherIndex constructKey mx
                                   | curOtherIndex >= maxOtherIndex = mx
                                   | otherwise = moveValuesHelper (curOtherIndex + 1) fromIndex toIndex maxOtherIndex constructKey $ moveValuesLine fromIndex constructKey curOtherIndex toIndex mx

                     moveValuesLine :: Int -> (Int -> Int -> (Int, Int)) -> Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
                     moveValuesLine index constructKey fixedIndex maxIndex mx
                            | index >= maxIndex = mx
                            | member old_index mx = moveValuesLine (index + 1) constructKey fixedIndex maxIndex new_mx
                                   where
                                          old_index = constructKey fixedIndex index
                                          new_index = constructKey fixedIndex (index - 1)
                                          value = mx ! old_index
                                          new_mx = Data.Map.insert new_index value $ delete old_index mx

       -- size :: SparseMatrix Int -> (Int, Int)
       size mx = (xMax, yMax)
              where
                     xMax = sparseMatrixHeight mx
                     yMax = sparseMatrixWidth mx

       -- row :: SparseMatrix Int -> Int -> [Int]
       row mx x = getByIndexSparse elems (x, 0) moveByY $ overflowed $ Part6.Tasks.size mx
              where
                     moveByY (x, y) = (x, y + 1)
                     elems = sparseMatrixElements mx

       -- column :: SparseMatrix Int -> Int -> [Int]
       column mx y = getByIndexSparse elems (0, y) moveByX $ overflowed $ Part6.Tasks.size mx
              where
                     moveByX (x, y) = (x + 1, y)
                     elems = sparseMatrixElements mx
       
       -- insert :: SparseMatrix Int -> (Int, Int) -> Int -> SparseMatrix Int
       insert mx coords@(x, y) newValue = SparseMatrix { sparseMatrixWidth=yMax, sparseMatrixHeight=xMax, sparseMatrixElements=newElems }
              where
                     newElems = Data.Map.insert coords newValue $ sparseMatrixElements mx
                     (xMax, yMax) = Part6.Tasks.size mx


overflowed :: (Int, Int) -> (Int, Int) -> Bool
overflowed (xMax, yMax) (x, y) = xMax <= x || yMax <= y

getByIndexSparse :: Map (Int, Int) Int -> (Int, Int) -> ((Int, Int) -> (Int, Int)) -> ((Int, Int) -> Bool) -> [Int]
getByIndexSparse mx index iterateF indexOverflowedCheck
       | indexOverflowedCheck index = []
       | otherwise = (findWithDefault 0 index mx) : getByIndexSparse mx (iterateF index) iterateF indexOverflowedCheck


-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = eyeHelper (mxzero w w) 0 w
       where
              eyeHelper :: Matrix m => m -> Int -> Int -> m
              eyeHelper mx curIndex maxIndex
                     | curIndex >= maxIndex = mx
                     | otherwise = eyeHelper (Part6.Tasks.insert mx (curIndex, curIndex) 1) (curIndex + 1) maxIndex
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = mxzero w h
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
