{-# LANGUAGE UnicodeSyntax #-}

module Diff (ConvertTVBandDA, diff, pushforward)
where

import Prelude.Unicode
import DifferentialAlgebra (DA)
import qualified DifferentialAlgebra as DA (lift)
import TangentVectorBundle (TVB)
import qualified TangentVectorBundle as TVB (tangent, bundle)
import ConvertTVBandDA (ConvertTVBandDA, toDA, toTVB)

-- ACTUAL DERIVATIVE-TAKING OPERATORS!
-- (Well, scaffolding and types.)

pushforward ∷ (TVB a a' ta, DA a da ba,
               TVB b b' tb, DA b db bb,
               ConvertTVBandDA a a' ta da ba,
               ConvertTVBandDA b b' tb db bb)
              ⇒ (a→b)→(ta→tb)

-- This cannot actually work without a "∀" contaminating its signature
-- for the function argument, so that it can actually be lifted.
-- Which requires (among other things) RankNTypes.

pushforward f = toTVB ∘ DA.lift f ∘ toDA

-- One day ⅅ or ⅆ.
diff ∷ (TVB a a' ta,
        DA a da ba,
        Num a',
        ConvertTVBandDA a a' ta da ba,
        DA b db bb,
        TVB b b' tb,
        ConvertTVBandDA b b' tb db bb)
       ⇒ (a→b)→(a→b')

diff f = TVB.tangent ∘ pushforward f ∘ flip TVB.bundle 1

-- This won't work
--  ∫ dx f a b = dx ⋅ sum [f x | x←[a,a+dx..b]]
-- because ∫ is infix, and also because
--  all (<=1) [0,0.26..1] ≡ False
