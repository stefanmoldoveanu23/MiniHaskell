module NatClass where

import Prelude (Show (..), (<>), Num(fromInteger), undefined)
import qualified GHC.Natural as Natural

import MyPrelude
import BoolClass
import MaybeClass


class NatClass n where
    zero :: n
    succ :: n -> n
    iter :: (a -> a) -> a -> n -> a

instance NatClass Natural.Natural where
    zero = 0
    succ n = n `Natural.plusNatural` 1
    iter f i 0 = i
    iter f i n = f (iter f i (n `Natural.minusNatural` 1))


one :: NatClass n => n
one = succ zero


isZero :: NatClass n => n -> CBool
isZero n = iter (const false) true n


add :: NatClass n => n -> n -> n
add a b = iter succ b a


mul :: NatClass n => n -> n -> n
mul a b = iter (add b) zero a


exp :: NatClass n => n -> n -> n
exp a b = iter (mul a) one b


pred :: NatClass n => n -> CMaybe n
pred n = iter (maybe (just zero) (just . succ)) nothing n


sub :: NatClass n => n -> n -> CMaybe n
sub a b = iter (maybeBind pred) (just a) b


lt :: NatClass n => n -> n -> CBool
lt a b = maybe true (const false) (sub a b)


gt :: NatClass n => n -> n -> CBool
gt a b = maybe true (const false) (sub b a)


gte :: NatClass n => n -> n -> CBool
gte a b = not (lt a b)


lte :: NatClass n => n -> n -> CBool
lte a b = not (gt a b)

eq :: NatClass n => n -> n -> CBool
eq a b = (lte a b) && (gte a b)


max :: NatClass n => n -> n -> n
max a b = ite (gt a b) a b


newtype CNat = CNat { getCNat :: forall a . (a -> a) -> a -> a }

instance NatClass CNat where
    zero = let func f i = id i in CNat func
    succ n = let func f i = f (iter f i n) in CNat func
    iter f i n = getCNat n f i


fromNatClass :: (NatClass n, NatClass m) => n -> m
fromNatClass = iter succ zero


instance Show CNat where
    show cn = "C" <> show (fromNatClass cn :: Natural.Natural)

instance Num CNat where
    fromInteger n = fromNatClass (fromInteger n :: Natural.Natural)