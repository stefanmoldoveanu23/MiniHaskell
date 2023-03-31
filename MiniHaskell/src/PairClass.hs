module PairClass where

import Prelude (Show (..), (<>), undefined)
import qualified Data.Tuple as Tuple

import MyPrelude
import BoolClass
import MaybeClass


class PairClass p where
    pair :: a -> b -> p a b
    uncurry :: (a -> b -> c) -> p a b -> c

instance PairClass (,) where
    pair = (,)
    uncurry = Tuple.uncurry


curry :: (PairClass p) => (p a b -> c) -> a -> b -> c
curry f a b = f (pair a b)


fst :: (PairClass p) => p a b -> a
fst pair = uncurry const pair


snd :: (PairClass p) => p a b -> b
snd pair = uncurry (flip const) pair


newtype CPair a b = CPair { getCPair :: forall c . (a -> b -> c) -> c }

instance PairClass CPair where
    pair a b = let func f = f a b in CPair func
    uncurry f p = getCPair p f


fromPairClass :: (PairClass p, PairClass q) => p a b -> q a b
fromPairClass = uncurry pair


instance (Show a, Show b) => Show (CPair a b) where
    show cp = "<" <> show (fromPairClass cp :: (a, b)) <> ">"