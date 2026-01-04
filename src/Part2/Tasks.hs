module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) a b = case (a, b) of 
   (IntConstant { intValue = v1 }, IntConstant {intValue = v2 }) -> IntConstant { intValue = v1 + v2}
   _ -> BinaryTerm { op = Plus, lhv = a, rhv = b }
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) a b = case (a, b) of 
   (IntConstant { intValue = v1 }, IntConstant {intValue = v2 }) -> IntConstant { intValue = v1 - v2}
   _ -> BinaryTerm { op = Minus, lhv = a, rhv = b }
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) a b = case (a, b) of 
   (IntConstant { intValue = v1 }, IntConstant {intValue = v2 }) -> IntConstant { intValue = v1 * v2}
   _ -> BinaryTerm { op = Times, lhv = a, rhv = b }
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expr@(IntConstant _) = expr
replaceVar varName replacement expr@(Variable {varName=x})
   | varName == x = replacement
   | otherwise = expr
replaceVar varName replacement (BinaryTerm { op=op, lhv=lhv, rhv=rhv}) = BinaryTerm {op=op, lhv=newLHV, rhv=newRHV}
   where 
      newLHV = replaceVar varName replacement lhv
      newRHV = replaceVar varName replacement rhv

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm {op=op, lhv=lhv, rhv=rhv}) = result 
   where
      leftValue = evaluate lhv
      rightValue = evaluate rhv

      result = case op of 
         Plus -> leftValue |+| rightValue
         Minus -> leftValue |-| rightValue
         Times -> leftValue |*| rightValue
evaluate expr = expr
