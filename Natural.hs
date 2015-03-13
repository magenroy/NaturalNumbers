module Natural (
    Natural(..),
    fromNat,
    toNat,
    leftAdd,
    rightAdd,
    leftMult,
    rightMult
) where

import Control.Exception (throw, ArithException (DivideByZero))
import Data.Ix (Ix, range, inRange)
import Data.Ratio ((%))

data Natural = Zero | Succ Natural deriving (Show, Read)

fromNat :: (Enum n, Num n) => Natural -> n
fromNat Zero = 0
fromNat (Succ n) = succ $ fromNat n

toNat :: (Ord n, Enum n, Num n) => n -> Natural
toNat 0 = Zero
toNat n | n > 0 = Succ $ toNat $ pred n
        | otherwise = Zero -- -|n| -> 0

instance Enum Natural where
    toEnum = toNat
    fromEnum = fromNat

instance Eq Natural where
    Zero == Zero = True
    Zero == Succ _ = False
    Succ _ == Zero = False
    Succ x == Succ y = x == y

instance Ord Natural where
    compare Zero Zero = EQ
    Zero `compare` Succ _ = LT
    Succ _ `compare` Zero = GT
    Succ x `compare` Succ y = compare x y

-- addition to the left side optimized for situations where x >= y
-- (has better constants than (+))
leftAdd x Zero = x
leftAdd x (Succ y) = leftAdd (Succ x) y

-- addition to the right side optimized for situations where x <= y
-- (has better constants than (+))
rightAdd Zero y = y
rightAdd (Succ x) y = rightAdd x $ Succ y

-- multiplication of the left side optimized for situations where x <= y
-- (has better constants than (*))
leftMult Zero _ = Zero
leftMult (Succ x) y = x + rightMult x y

-- multiplication of the right side optimized for situations where x >= y
-- (has better constants than (*))
rightMult _ Zero = Zero
rightMult x (Succ y) = leftMult x y + y

instance Num Natural where
    x + y = go (x, y) (x, y)
	where go (Zero, n) _ = n
	      go _ (n, Zero) = n
	      go (Succ x1, y1) (x2, Succ y2) = go (x1, Succ y1) (Succ x2, y2)

    x - Zero = x
    Zero - _ = Zero -- Zero - x = Zero, or should this be an error?
    Succ x - Succ y = x - y

    x * y = go (x, Zero) (y, Zero)
	where go (Zero, n) _ = n
	      go _ (Zero, n) = n
	      go (Succ x, a) (Succ y, b) = go (x, a + y) (y, b + x)

    abs = id -- always positive
    signum _ = Succ Zero -- always positive
    fromInteger = toNat

instance Real Natural where
    toRational n = toInteger n % 1

instance Integral Natural where
    quotRem _ Zero = throw DivideByZero
    quotRem Zero _ = (Zero, Zero)
    quotRem x y | x < y = (Zero, x)
		| otherwise = uncurry ((,) . Succ) $ quotRem (x - y) y
    toInteger = fromNat

instance Ix Natural where
    range = uncurry enumFromTo
    inRange (min, max) n = min <= n && n <= max
