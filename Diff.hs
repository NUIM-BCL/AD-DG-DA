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

pushforward ∷ (TVB tag a a' ta, DA tag a da ba,
               TVB tag b b' tb, DA tag b db bb,
               ConvertTVBandDA tag a a' ta da ba,
               ConvertTVBandDA tag b b' tb db bb)
              ⇒ (a→b)→(ta→tb)

-- This cannot actually work without a "∀" contaminating its signature
-- for the function argument, so that it can actually be lifted.
-- Which requires (among other things) RankNTypes.

pushforward f = toTVB ∘ DA.lift f ∘ toDA

-- One day ⅅ or ⅆ.
diff ∷ (TVB tag a a' ta,
        DA tag a da ba,
        Num a',
        ConvertTVBandDA tag a a' ta da ba,
        DA tag b db bb,
        TVB tag b b' tb,
        ConvertTVBandDA tag b b' tb db bb)
       ⇒ (a→b)→(a→b')

diff f = TVB.tangent ∘ pushforward f ∘ flip TVB.bundle 1

-- This won't work
--  ∫ dx f a b = dx ⋅ sum [f x | x←[a,a+dx..b]]
-- because ∫ is infix, and also because
--  all (<=1) [0,0.26..1] ≡ False
