{-# LANGUAGE UnicodeSyntax #-}

module Diff (ConvertTBandDA, diff, pushforward, forwardAD)
where

import Prelude.Unicode
import qualified DifferentialAlgebra as DA (lift)
import qualified TangentBundle as TB (tangent, bundle, unbundle)
import ConvertTBandDA (ConvertTBandDA, toDA, toTB)

-- ACTUAL DERIVATIVE-TAKING OPERATORS!
-- (Well, scaffolding and types.)

pushforward ∷ (ConvertTBandDA a a' ta da ba,
               ConvertTBandDA b b' tb db bb)
              ⇒ (a → b) → (ta → tb)

-- This cannot actually work without a "∀" contaminating its signature
-- for the function argument, so that it can actually be lifted.
-- Which requires (among other things) RankNTypes.  Or, a reflective
-- mechanism to implement DA.lift of a function object.

pushforward f = toTB ∘ DA.lift f ∘ toDA

-- One day ⅅ or ⅆ.
diff ∷ (Num a',
        ConvertTBandDA a a' ta da ba,
        ConvertTBandDA b b' tb db bb)
       ⇒ (a → b) → (a → b')

diff f = TB.tangent ∘ pushforward f ∘ flip TB.bundle 1

-- This won't work
--  ∫ dx f a b = dx ⋅ sum [f x | x←[a,a+dx..b]]
-- because ∫ is infix, and also because
--  all (<=1) [0,0.26..1] ≡ False


-- This is a "conventional" API for forward AD.

forwardAD ∷ (ConvertTBandDA a a' ta da ba,
             ConvertTBandDA b b' tb db bb)
            ⇒ (a → b) → a → a' → (b, b')

forwardAD f x = TB.unbundle ∘ pushforward f ∘ TB.bundle x
