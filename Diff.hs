{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Diff (ConvertTVBandDA, diff, pushforward)
where

import Prelude.Unicode
import DifferentialAlgebra (DA)
import qualified DifferentialAlgebra as DA (lift, primal)
import TangentVectorBundle (TVB)
import qualified TangentVectorBundle as TVB (tangent, bundle)
import Numeric.Dual (Dual)

-- Need to define converters DA to/from TVB, in order to implement DG
-- via DA.

class ConvertTVBandDA a a' ta da ba
 | a→a' ta da ba,
   ta→a,
   ba→a
 where
  fromTVBtoDA ∷  (TVB a a' ta, DA a da ba) ⇒ ta→ba
  fromDAtoTVB ∷  (TVB a a' ta, DA a da ba) ⇒ ba→ta

instance ConvertTVBandDA Double Double (Dual tag Double) Double (Dual tag Double)
 where
  fromTVBtoDA = id
  fromDAtoTVB = id

-- instance (Num a, TVB a a (Dual tag a), DA a a (Dual tag a))
--          ⇒ ConvertTVBandDA a a (Dual tag a) a (Dual tag a) where
--   fromTVBtoDA = id
--   fromDAtoTVB = id

instance (TVB a a' ta,
          DA a da ba,
          ConvertTVBandDA a a' ta da ba,
          TVB b b' tb,
          DA b db bb,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (a→b) (a→b') (a→tb) (da→db) (ba→bb)
 where
  fromDAtoTVB f = fromDAtoTVB ∘ f ∘ DA.lift
  fromTVBtoDA f = fromTVBtoDA ∘ f ∘ DA.primal -- *unsafe* unless tangent is zero

instance (TVB a a' ta,
          DA a da ba,
          ConvertTVBandDA a a' ta da ba,
          TVB b b' tb,
          DA b db bb,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (a,b) (a',b') (ta,tb) (da,db) (ba,bb)
 where
  fromDAtoTVB (x,y) = (fromDAtoTVB x, fromDAtoTVB y)
  fromTVBtoDA (x,y) = (fromTVBtoDA x, fromTVBtoDA y)

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

pushforward f = fromDAtoTVB ∘ DA.lift f ∘ fromTVBtoDA

diff ∷ (TVB a a' ta,
        DA a da ba,
        Num a',
        ConvertTVBandDA a a' ta da ba,
        DA b db bb,
        TVB b b' tb,
        ConvertTVBandDA b b' tb db bb)
       ⇒ (a→b)→(a→b')

diff f = TVB.tangent ∘ pushforward f ∘ flip TVB.bundle 1
