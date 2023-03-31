module MaybeClass where

import Prelude (Show (..), (<>), undefined)
import qualified Data.Maybe as Maybe
import qualified Data.Bool as Bool

import MyPrelude
import BoolClass


class MaybeClass m where
    nothing :: m a
    just :: a -> m a
    maybe :: b -> (a -> b) -> m a -> b

instance MaybeClass Maybe.Maybe where
    nothing = Maybe.Nothing
    just = Maybe.Just
    maybe = Maybe.maybe


fromMaybe :: MaybeClass m => a -> m a -> a
fromMaybe def mab = maybe def id mab


isNothing :: (MaybeClass m, BoolClass b) => m a -> b
isNothing mab = maybe true (const false) mab


isJust :: (MaybeClass m, BoolClass b) => m a -> b
isJust mab = not (isNothing mab)


maybeFMap :: MaybeClass m => (a -> b) -> m a -> m b
maybeFMap f mab = maybe nothing (just . f) mab


maybeBind :: MaybeClass m => (a -> m b) -> m a -> m b
maybeBind f mab = maybe nothing f mab


newtype CMaybe a = CMaybe { getCMaybe :: forall b . b -> (a -> b) -> b }

instance MaybeClass CMaybe where
    nothing = CMaybe const
    just x = let func _ f = f x in CMaybe func
    maybe n j m = getCMaybe m n j

fromMaybeClass :: (MaybeClass m, MaybeClass n) => m a -> n a
fromMaybeClass = maybe nothing just


instance Show a => Show (CMaybe a) where
    show cm = "C" <> show (fromMaybeClass cm :: Maybe.Maybe a)