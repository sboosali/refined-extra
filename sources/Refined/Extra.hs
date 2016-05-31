{-# LANGUAGE NamedFieldPuns, TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, KindSignatures, ScopedTypeVariables #-}
module Refined.Extra where
import Refined.Internal
import Refined

import GHC.TypeLits
import Data.Proxy
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)
import Data.Function ((&))

--------------------------------------------------------------------------------
-- Between

type Between (min :: Nat) (max :: Nat) = Refined (FromTo min max)

type IsBetween (min :: Nat) (max :: Nat) a = (Num a, Ord a, KnownNat min, KnownNat max, min <= max)

instance
 (IsBetween min max a)
 =>
 Bounded (Between min max a)
  where

  minBound = unsafeBetween _minimum
   where
     _minimum :: a
     _minimum = natVal (Proxy::Proxy min) & fromIntegral

  maxBound = unsafeBetween _maximum
   where
     _maximum :: a
     _maximum = natVal (Proxy::Proxy max) & fromIntegral

unsafeBetween :: (IsBetween min max a) => a -> Between min max a
unsafeBetween = refine >>> either2maybe >>> fromJust -- TODO callstack

--------------------------------------------------------------------------------
-- Fraction

{- | a real number on the unit interval, @[0,1]@
(i.e. between zero and one, inclusive).

<http://nikita-volkov.github.io/refined/>

<http://stackoverflow.com/questions/30531944/a-type-thats-easy-to-do-arithmetic-with-and-is-guaranteed-in-bounds>

-}
type Fraction = Between 0 1 Double

-- instance Bounded Fraction where
--   minBound = unsafeFraction 0
--   maxBound = unsafeFraction 1

unsafeFraction :: Double -> Fraction
unsafeFraction = unsafeBetween

isFraction :: Double -> Maybe Fraction
isFraction = refine >>> either2maybe

-- refineTH :: (Predicate p x, Lift x) => x -> Q (TExp (Refined p x))

--------------------------------------------------------------------------------
-- Aliases

type (:&:) = And
type (:|:) = Or
type (:<:) = LessThan
type (:>:) = GreaterThan

--------------------------------------------------------------------------------
