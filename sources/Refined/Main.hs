{-# LANGUAGE QuasiQuotes #-}
module Refined.Main where
import Refined.Fraction

{- | @
stack build && stack exec -- refined-extra-example
@-}
main = do
 print [fraction|0.5|]
