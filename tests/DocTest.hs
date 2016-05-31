{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.DocTest

main = doctest
 [ "sources/Refined/Fraction.hs"
 , "sources/Refined/Extra.hs"
 ]
