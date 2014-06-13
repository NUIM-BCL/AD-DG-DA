{-# LANGUAGE UnicodeSyntax #-}

module Diff (ConvertTVBandDA, diff, pushforward, forwardAD)
where

import Prelude.Unicode
import qualified DifferentialAlgebra as DA (lift)
import qualified TangentVectorBundle as TVB (tangent, bundle, unbundle)
import ConvertTVBandDA (ConvertTVBandDA, toDA, toTVB)

-- ACTUAL DERIVATIVE-TAKING OPERATORS!
-- (Well, scaffolding and types.)

pushforward ∷ (ConvertTVBandDA a a' ta da ba,
               ConvertTVBandDA b b' tb db bb)
              ⇒ (a→b)→(ta→tb)

-- This cannot actually work without a "∀" contaminating its signature
-- for the function argument, so that it can actually be lifted.
-- Which requires (among other things) RankNTypes.

pushforward f = toTVB ∘ DA.lift f ∘ toDA

-- One day ⅅ or ⅆ.
diff ∷ (Num a',
        ConvertTVBandDA a a' ta da ba,
        ConvertTVBandDA b b' tb db bb)
       ⇒ (a → b) → (a → b')

diff f = TVB.tangent ∘ pushforward f ∘ flip TVB.bundle 1

-- This won't work
--  ∫ dx f a b = dx ⋅ sum [f x | x←[a,a+dx..b]]
-- because ∫ is infix, and also because
--  all (<=1) [0,0.26..1] ≡ False


-- This is a "conventional" API for forward AD.

forwardAD ∷ (ConvertTVBandDA a a' ta da ba,
             ConvertTVBandDA b b' tb db bb)
            ⇒ (a → b) → a → a' → (b, b')

forwardAD f x = TVB.unbundle ∘ pushforward f ∘ TVB.bundle x
