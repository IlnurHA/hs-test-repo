{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map
import Data.Maybe

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
       mxsubmx :: mx -> Int -> Int -> Maybe mx
       
       --
       mxsize :: mx -> (Int, Int)

       --
       mxrow :: mx -> Int -> Maybe [Int]
       mxcolumn :: mx -> Int -> Maybe [Int]
       
       --
       mxinsert :: mx -> Int -> Int -> Int -> mx
       mxget :: mx -> Int -> Int -> Maybe Int

       -- Utility function for Dima's tests
       fromList2D :: [[Int]] -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       mxzero 1 1 = 0
       mxzero _ _ = error "Wrong dimensions. Expected 1 1"

       mxsubmx _ _ _ = Nothing
       mxsize _ = (1, 1)

       mxrow mx 0    = Just [mx]
       mxrow _ _     =  Nothing

       mxcolumn mx 0 = Just [mx]
       mxcolumn _ _  = Nothing

       mxinsert _ 0 0 value = value
       mxinsert mx _ _ _ = mx

       mxget mx 0 0 = Just mx
       mxget _ _ _ = Nothing

       fromList2D [[n]] = n
       fromList2D _ = error "Wrong dimensions"


elementAt :: Int -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt 0 (x:_) = Just x
elementAt n (_:xs) = elementAt (n - 1) xs

instance Matrix [[Int]] where
       mxzero x y = replicate x (replicate y 0)
       
       mxsubmx mx x y = Just $ [[elem | (elem, yIndex) <- zip row [0..], yIndex /= y] | (row, xIndex) <- zip mx [0..], xIndex /= x]

       mxsize [] = (0, 0)
       mxsize arr@([]:xs) = (length arr, 0)
       mxsize arr@(x:xs) = (length arr, length x)

       mxrow mx x = elementAt x mx
       mxcolumn mx y = Just $ [ elem | Just elem <- (Prelude.map (elementAt y) mx) ]

       mxinsert mx x y newValue = [[ chooseValue value newValue xIndex yIndex | (value, yIndex) <- zip row [0..]] | (row, xIndex) <- zip mx [0..]]
              where chooseValue oldValue newValue xIndex yIndex
                     | xIndex == x && yIndex == y = newValue
                     | otherwise = oldValue
       
       mxget mx x y = elementAt x mx >>= elementAt y

       fromList2D arr = arr

instance Matrix (SparseMatrix Int) where
       mxzero x y = SparseMatrix { sparseMatrixWidth=y, sparseMatrixHeight=x, sparseMatrixElements=Data.Map.empty }

       mxsubmx mx x y = Just $ SparseMatrix { sparseMatrixWidth=yMax - 1, sparseMatrixHeight=xMax - 1, sparseMatrixElements=newElems }
              where
                     elems :: Map (Int, Int) Int
                     elems = sparseMatrixElements mx 
                     xMax :: Int
                     xMax = sparseMatrixHeight mx
                     yMax :: Int
                     yMax = sparseMatrixWidth mx

                     newElems :: Map (Int, Int) Int
                     -- newElems = error (show $ indeciesToDelete x y)
                     -- newElems = deleteElems elems x y
                     newElems = moveKeys (deleteElems elems x y) x y

                     deleteElems :: Map (Int, Int) Int -> Int -> Int -> Map (Int, Int) Int
                     deleteElems mx x y = deleteHelper indecies mx
                            where
                                   indecies = indeciesToDelete x y

                                   deleteHelper [] mx = mx
                                   deleteHelper (x:xs) mx = deleteHelper xs (delete x mx)

                     indeciesToDelete :: Int -> Int -> [(Int, Int)]
                     indeciesToDelete x y = Prelude.foldl (<>) [] [[ (xIndex, yIndex) | yIndex <- [0..yMax - 1], isNecessaryIndex xIndex yIndex ] | xIndex <- [0..xMax - 1]]
                            where
                                   isNecessaryIndex xIndex yIndex = xIndex == x || yIndex == y
                     
                     moveKeys :: Map (Int, Int) Int -> Int -> Int -> Map (Int, Int) Int
                     moveKeys mx x y = mapKeys moveK mx
                            where 
                                   moveK (xIndex, yIndex)
                                          | xIndex > x && yIndex > y = (xIndex - 1, yIndex - 1)
                                          | xIndex > x = (xIndex - 1, yIndex)
                                          | yIndex > y = (xIndex, yIndex - 1)
                                          | otherwise = (xIndex, yIndex)

       mxsize mx = (xMax, yMax)
              where
                     xMax = sparseMatrixHeight mx
                     yMax = sparseMatrixWidth mx

       -- mxrow :: mx -> Int -> Maybe [a]
       mxrow mx x 
              | x >= xMax || x < 0 = Nothing
              | otherwise = Just $ [findWithDefault 0 (x, yIndex) elems | yIndex <- [0..yMax - 1]]
              where
                     xMax = sparseMatrixHeight mx
                     yMax = sparseMatrixWidth mx
                     elems = sparseMatrixElements mx

       -- mxcolumn :: mx -> Int -> Maybe [a]
       mxcolumn mx y 
              | y >= yMax || y < 0 = Nothing
              | otherwise = Just $ [findWithDefault 0 (xIndex, y) elems | xIndex <- [0..xMax - 1]]
              where
                     xMax = sparseMatrixHeight mx
                     yMax = sparseMatrixWidth mx
                     elems = sparseMatrixElements mx
       
       -- mxinsert :: mx -> Int -> Int -> a -> mx
       mxinsert mx x y value
              | x >= xMax || x < 0 || y >= yMax || y < 0 = mx
              | otherwise = SparseMatrix { sparseMatrixWidth=yMax, sparseMatrixHeight=xMax, sparseMatrixElements=(insert (x, y) value elems) }
              where
                     xMax = sparseMatrixHeight mx
                     yMax = sparseMatrixWidth mx
                     elems = sparseMatrixElements mx
       
       -- mxget :: mx -> Int -> Int -> Maybe a
       mxget mx x y
              | x >= xMax || x < 0 || y >= yMax || y < 0 = Nothing
              | otherwise = Just $ fromMaybe 0 $ elems !? (x, y)
              where
                     xMax = sparseMatrixHeight mx
                     yMax = sparseMatrixWidth mx
                     elems = sparseMatrixElements mx

       fromList2D arr = SparseMatrix { sparseMatrixWidth=columnLength, sparseMatrixHeight=rowLength, sparseMatrixElements=newElems }
              where
                     newElems = insertHelper Data.Map.empty convertedValues
                     rowLength = length arr
                     columnLength = case elementAt 0 arr of
                                   Nothing -> 0
                                   Just x -> length x

                     convertedValues = Prelude.foldl (<>) [] [ [ ((xIndex, yIndex), elem) | (elem, yIndex) <- zip row [0..]] | (row, xIndex) <- zip arr [0..]]
                     
                     insertHelper mx [] = mx
                     insertHelper mx ((x, value):xs) = insertHelper (insert x value mx) xs

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = eyeHelper (mxzero w w) 0 w
       where
              eyeHelper :: Matrix m => m -> Int -> Int -> m
              eyeHelper mx curIndex maxIndex
                     | curIndex > maxIndex = mx 
                     | otherwise = eyeHelper (mxinsert mx curIndex curIndex 1) (curIndex + 1) maxIndex
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = mxzero h w
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix mx1 mx2 =
       case resultingSize of 
              Nothing -> error "Not compatible matrices"
              Just (newX, newY) -> multiply mx1 mx2 (mxzero newX newY) newX newY
       where
              resultingSize = case (mxsize mx1, mxsize mx2) of
                                   ((x1, y1), (x2, y2)) | y1 == x2 -> Just (x1, y2)
                                   otherwise -> Nothing
              multiply mx1 mx2 newMX newX newY = applyOperations newMX $ operations mx1 mx2 newX newY
              
              operations mx1 mx2 newX newY = Prelude.foldl (<>) [] [[((x, y), result mx1 mx2 x y) | y <- [0..newY - 1]] | x <- [0..newX - 1]]
                     where
                            result mx1 mx2 x y = sum $ zipWith (\r -> \c -> r * c) (row mx1 x) (column mx2 y) 
                            row mx x = fromJust $ mxrow mx x
                            column mx y = fromJust $ mxcolumn mx y

              applyOperations mx [] = mx
              applyOperations mx (((x, y), value):xs) = applyOperations (mxinsert mx x y value) xs 
-- Определитель матрицы
determinant :: (Show m, Matrix m) => m -> Int
determinant mx
       | not $ isSquare mx = error ("Determinant available only for square matrices: " ++ (show mx))
       | mxsize mx == (1, 1) = fromJust $ mxget mx 0 0
       | otherwise = sum $ [((-1) ^ (yIndex `mod` 2)) * (var yIndex) * (determinant $ submatrix yIndex) | yIndex <- [0..yMax - 1]]
       where
              var y = fromJust $ mxget mx 0 y
              submatrix y = fromJust $ mxsubmx mx 0 y
              (xMax, yMax) = mxsize mx
              isSquare mx = x == y
                     where (x, y) = mxsize mx