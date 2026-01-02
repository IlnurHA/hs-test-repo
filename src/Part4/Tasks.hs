module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = listToRlistHelper REmpty

listToRlistHelper :: ReverseList a -> [a] -> ReverseList a
listToRlistHelper newArr [] = newArr
listToRlistHelper newArr (x:xs) = listToRlistHelper (newArr :< x) xs

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ (REmpty) = showString ""
    showsPrec _ (REmpty :< x) = shows x
    showsPrec _ (xs :< x) =
        showsPrec 0 xs .
        showString "," .
        shows x
    show x = "[" ++ (showsPrec 0 x "") ++ "]"
instance Eq (ReverseList a) where
    (==) = notImplementedYet
    (/=) = notImplementedYet
instance Semigroup (ReverseList a) where
instance Monoid (ReverseList a) where
instance Functor ReverseList where
instance Applicative ReverseList where
instance Monad ReverseList where
