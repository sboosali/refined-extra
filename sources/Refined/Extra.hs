{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, KindSignatures, ScopedTypeVariables #-}
{-| Extensions to the @refined@ package.

Background:

* <http://nikita-volkov.github.io/refined/>

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

unsafeBetween :: (IsBetween min max a) => a -> Between min max a
unsafeBetween = refine >>> unsafeFromRight -- TODO callstack

--------------------------------------------------------------------------------
-- Aliases

type (:&:) = And
type (:|:) = Or
type (:<:) = LessThan
type (:>:) = GreaterThan

--------------------------------------------------------------------------------
