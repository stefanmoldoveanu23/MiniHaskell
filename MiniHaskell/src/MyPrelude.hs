module MyPrelude where

import Prelude ()


id :: a -> a
id a = a


const :: a -> b -> a
const x _ = x


other :: a -> b -> b
other _ y = y


flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x


(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)