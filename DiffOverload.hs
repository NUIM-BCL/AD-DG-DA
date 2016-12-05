{-# LANGUAGE UnicodeSyntax #-}

module DiffOverload (diff, forwardAD)
where

import Prelude.Unicode
import Numeric.Dual (Dual)
import qualified Numeric.Dual as Dual (bundle)
import qualified TangentBundle as TB (tangent, bundle, unbundle)
import ConvertTBandDA (ConvertTBandDA, toDA, toTB)

-- This version relies upon the type class system to ensure that the
-- function argument f is already lifted into the DA domain.

diff ∷ (Num a,
        ConvertTBandDA b b' tb db bb)
       ⇒ (Dual a → bb) → (a → b')

diff f = TB.tangent ∘ toTB ∘ f ∘ flip Dual.bundle 1

forwardAD ∷ (ConvertTBandDA a a' ta da ba,
             ConvertTBandDA b b' tb db bb)
       ⇒ (ba → bb) → a → a' → (b, b')

forwardAD f x x' = TB.unbundle $ toTB $ f $ toDA $ TB.bundle x x'
