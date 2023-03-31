module ListClass where

import Prelude (Show (..), (<>), undefined)
import qualified Data.List as List

import MyPrelude
import BoolClass
import MaybeClass
import NatClass
import PairClass


class ListClass l where
    nil :: l a
    cons :: a -> l a -> l a
    foldr :: (a -> b -> b) -> b -> l a -> b

instance ListClass [] where
    nil = []
    cons = (:)
    foldr = List.foldr


(++) :: ListClass l => l a -> l a -> l a
l1 ++ l2 = foldr cons l2 l1


length :: ListClass l => l a -> CNat
length = foldr (\a b -> succ b) zero


isNull :: ListClass l => l a -> CBool
isNull = isZero . length


map :: ListClass l => (a -> b) -> l a -> l b
map f = foldr (\a b -> cons (f a) b) nil


filter :: ListClass l => (a -> CBool) -> l a -> l a
filter f = foldr (\a b -> ite (f a) (cons a b) b) nil


foldl :: ListClass l => (b -> a -> b) -> b -> l a -> b
foldl f e l = (foldr (\a b -> b . ((flip f) a)) id l) e


uncons :: ListClass l => l a -> CMaybe (CPair a (l a))
uncons = foldr (\a b -> maybe (just (pair a nil)) (\p -> just (pair a (cons (fst p) (snd p)))) b) nothing


head :: ListClass l => l a -> CMaybe a
head l = maybe nothing (\p -> just (fst p)) (uncons l)


tail :: ListClass l => l a -> CMaybe (l a)
tail l = maybe nothing (\p -> just (snd p)) (uncons l)


reverse :: ListClass l => l a -> l a
reverse = foldl (flip cons) nil


sum :: ListClass l => l CNat -> CNat
sum = foldr add zero


product :: ListClass l => l CNat -> CNat
product = foldr mul one


maximum :: ListClass l => l CNat -> CNat
maximum = foldr max zero


natToList :: ListClass l => CNat -> l CNat
natToList n = ite (isZero n) nil (cons n (natToList (fromMaybe zero (pred n))))


newtype CList a = CList { getCList :: forall b . (a -> b -> b) -> b -> b }

instance ListClass CList where
    nil = CList (\f b -> b)
    cons a l = CList (\f b -> f a (getCList l f b))
    foldr f i l = getCList l f i


fromListClass :: (ListClass l1, ListClass l2) => l1 a -> l2 a
fromListClass = foldr cons nil


instance (Show a) => Show (CList a) where
    show cl = "{" <> show (fromListClass cl :: [a]) <> "}"


factorial :: CNat -> CNat
factorial n = product ((natToList n) :: CList CNat)