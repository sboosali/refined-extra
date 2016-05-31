{-# LANGUAGE RecordWildCards #-}
module Refined.Internal where

-- import Refined

import Language.Haskell.TH
import Language.Haskell.TH.Quote

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

{-|

@
qq :: QuasiQuoter
qq = defaultQuasiQuoter{ quoteExp = quoteExp }
 where
 quoteExp s = do
  ...
@

-}
defaultQuasiQuoter :: QuasiQuoter
defaultQuasiQuoter = QuasiQuoter{..}
 where

 quoteExp  :: String -> Q Exp
 quoteExp  = error $ message "expressions"

 quotePat  :: String -> Q Pat
 quotePat  = error $ message "patterns"

 quoteType :: String -> Q Type
 quoteType = error $ message "types"

 quoteDec  :: String -> Q [Dec]
 quoteDec  = error $ message "declarations"

 message s = "this QuasiQuoter doesn't support " ++ s
