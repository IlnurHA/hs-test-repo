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
instance Eq a => Eq (ReverseList a) where
    (==) = \lhs -> \rhs -> case (lhs, rhs) of
            (REmpty, REmpty) -> True
            ((xs :< x), (ys :< y)) -> x == y && xs == ys
            otherwise -> False
    (/=) = \lhs -> \rhs -> not (lhs == rhs)
instance Semigroup (ReverseList a) where
    -- (<>) :: a -> a -> a
    (<>) lhs REmpty = lhs
    (<>) lhs (xs :< x) = (lhs <> xs) :< x
instance Monoid (ReverseList a) where
    mempty = REmpty
    mconcat = foldl (<>) mempty
instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (xs :< x) = (fmap f xs) :< (f x)
instance Applicative ReverseList where
    pure x = REmpty :< x
    (<*>) REmpty _ = REmpty
    (<*>) (fs :< f) arr = (fs <*> arr) <> fmap f arr
instance Monad ReverseList where
    return = pure
    arr >>= f = myFlatten (fmap f arr)
        where
            myFlatten REmpty = REmpty
            myFlatten (xs :< x) = (myFlatten xs) <> x
