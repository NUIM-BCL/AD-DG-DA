{-# LANGUAGE UnicodeSyntax #-}

module DiffOverload (diff)
where

import Prelude.Unicode
import Numeric.Dual (Dual)
import qualified Numeric.Dual as Dual (bundle)
import qualified TangentVectorBundle as TVB (tangent)
import ConvertTVBandDA (ConvertTVBandDA, toTVB)

-- This version relies upon the type class system to ensure that the
-- function argument f is already lifted into the DA domain.

diff ∷ (Num a,
        ConvertTVBandDA b b' tb db bb)
       ⇒ (Dual a → bb) → (a → b')

diff f = TVB.tangent ∘ toTVB ∘ f ∘ flip Dual.bundle 1
