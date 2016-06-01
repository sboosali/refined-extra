{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, KindSignatures, ScopedTypeVariables, ViewPatterns #-}
{-| Extensions to the @refined@ package.

Background:

* <http://nikita-volkov.github.io/refined/>
* <http://stackoverflow.com/questions/30531944/a-type-thats-easy-to-do-arithmetic-with-and-is-guaranteed-in-bounds>

-}
module Refined.Extra where
import Refined.Internal
import Refined

import GHC.TypeLits
import Data.Proxy

import Control.Arrow ((>>>))
import Data.Function ((&))

--------------------------------------------------------------------------------
-- Between

type Between (min :: Nat) (max :: Nat) = Refined (FromTo min max)

type IsBetween (min :: Nat) (max :: Nat) a =
   ( Num a
   , Ord a
   , KnownNat min
   , KnownNat max
   , min <= max
   )

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

-- | TODO make less stupid, or remove. implement interfaces from a finer numerical hierarchy.
instance (IsBetween min max a) => Num (Between min max a) where
  (+) = bounded2 (+)
  (-) = bounded2 (-)
  (*) = bounded2 (*) -- multiplication preserves the bounds

  -- (/) = bounded2 (/)

  fromInteger = fromInteger >>> boundedBetween --TODO but this means that {{ 0.25 * 2 }} == {{ 0.25 }} not {{ 0.5 }} bc 2 is bounded to 1.

  negate = id -- bounded1 negate
  abs    = id -- bounded1 abs
  signum = id -- bounded1 signum

unsafeBetween :: (IsBetween min max a) => a -> Between min max a
unsafeBetween = refine >>> unsafeFromRight -- TODO callstack

getBetween :: Between min max a -> a
getBetween = unrefine

boundedBetween :: (IsBetween min max a) => a -> Between min max a
boundedBetween = unsafeBetween >>> bounded

-- | enforce the invariant with 'min' and 'max'.
bounded :: (IsBetween min max a) => Between min max a -> Between min max a
bounded = min minBound >>> max maxBound

-- | lift a unary function, bounding the output.
bounded1
 :: (IsBetween min max a)
 => (a -> a)
 -> (Between min max a -> Between min max a)
bounded1 f
 = getBetween >>> f >>> boundedBetween

-- | lift a binary function, bounding the output.
bounded2
 :: (IsBetween min max a)
 => (a -> a -> a)
 -> (Between min max a -> Between min max a -> Between min max a)
bounded2 f (getBetween -> x) (getBetween -> y)
 = boundedBetween (f x y)

-- | lift a ternary function, bounding the output.
bounded3
 :: (IsBetween min max a)
 => (a -> a -> a -> a)
 -> (Between min max a -> Between min max a -> Between min max a -> Between min max a)
bounded3 f (getBetween -> x) (getBetween -> y) (getBetween -> z)
 = boundedBetween (f x y z)

--------------------------------------------------------------------------------
-- Aliases

type (:&:) = And
type (:|:) = Or
type (:<:) = LessThan
type (:>:) = GreaterThan

--------------------------------------------------------------------------------
