{-# LANGUAGE TemplateHaskell, NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

{-|

Background:

* <http://stackoverflow.com/questions/30531944/a-type-thats-easy-to-do-arithmetic-with-and-is-guaranteed-in-bounds>

-}
module Refined.Fraction where
import Refined.Internal
import Refined.Extra
import Refined

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Read (readEither)

import Control.Arrow ((>>>))
import Data.Function ((&))


--------------------------------------------------------------------------------
-- Fraction

{- | a real number on the unit interval, @[0,1]@
(i.e. between zero and one, inclusive).

<http://nikita-volkov.github.io/refined/>

<http://stackoverflow.com/questions/30531944/a-type-thats-easy-to-do-arithmetic-with-and-is-guaranteed-in-bounds>

-}
type Fraction = Between 0 1 Double
-- newtype

unsafeFraction :: Double -> Fraction
unsafeFraction = unsafeBetween

isFraction :: Double -> Maybe Fraction
isFraction = refine >>> either2maybe

-- refineTH :: (Predicate p x, Lift x) => x -> Q (TExp (Refined p x))

{- |

dynamic refinement (with 'refine'):

>>> :set -XDataKinds
>>> refine 0.5 :: Either String (Between 0 1 Double)
Right (Refined 0.5)

static refinement (with 'refineTH'):

>>> :set -XDataKinds
>>> :set -XTemplateHaskell
>>> $$(refineTH 0.5) :: Between 0 1 Double
Refined 0.5

static refinement (with 'fraction'):

>>> :set -XDataKinds
>>> :set -XQuasiQuotes
>>> [fraction|0.5|]
Refined 0.5

-}
fraction :: QuasiQuoter
fraction = defaultQuasiQuoter{quoteExp}
 where
 quoteExp s = parseRefine s & either fail __splice__

 __splice__ :: Fraction -> Q Exp
 __splice__ x = [| x |]

 parseRefine :: String -> Either String Fraction
 parseRefine s = do
    a <- readEither s
    b <- refine a
    return b
