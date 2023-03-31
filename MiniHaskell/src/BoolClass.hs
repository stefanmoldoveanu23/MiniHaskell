module BoolClass where

import Prelude (Show (..), (<>), undefined)
import qualified Data.Bool as Bool

import MyPrelude


class BoolClass b where
    false :: b
    true :: b
    bool :: a -> a -> b -> a

instance BoolClass Bool.Bool where
    false = Bool.False
    true = Bool.True
    bool = Bool.bool


ite :: BoolClass b => b -> a -> a -> a
ite expr reta retb = bool retb reta expr


(&&) :: BoolClass b => b -> b -> b
expr1 && expr2 = bool false expr2 expr1


(||) :: BoolClass b => b -> b -> b
expr1 || expr2 = bool expr2 true expr1


not :: BoolClass b => b -> b
not expr = bool true false expr


newtype CBool = CBool { getCBool :: forall a. a -> a -> a}

instance BoolClass CBool where
    true = CBool const
    false = CBool (flip const)
    bool f t b = getCBool b t f


fromBoolClass :: (BoolClass a, BoolClass b) => a -> b
fromBoolClass = bool false true


instance Show CBool where
    show cb = "C" <> show (fromBoolClass cb :: Bool.Bool)


