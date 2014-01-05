{-# LANGUAGE UnicodeSyntax #-}

module TangentVectorBundle (bundle, unbundle, primal, tangent, vzero, vlift)
where

import Prelude.Unicode
import DifferentialAlgebra
import Numeric.Dual

-- Tangent vector bundle
class TVB a a' ta | ta→a, ta→a', a→a', a→ta where
  bundle ∷ a→a'→ta
  unbundle ∷ ta→(a,a')
  unbundle x = (primal x, tangent x)
  primal ∷ ta→a
  primal = fst ∘ unbundle
  tangent ∷ ta→a'
  tangent = snd ∘ unbundle
  vzero ∷ a→a'
  vlift ∷ a→ta
  vlift x = bundle x (vzero x)

-- instance Num a ⇒ TVB a a (Dual tag a) where
--   bundle = Dual
--   unbundle (Dual x x') = (x,x')
--   vzero = const 0

instance TVB Double Double (Dual tag Double) where
  bundle = Dual
  unbundle (Dual x x') = (x,x')
  vzero = const 0

-- Differential Geometric (DG) definition of the tangent vector bundle
-- of a function type.
instance TVB b b' tb ⇒ TVB (a→b) (a→b') (a→tb) where
  bundle f f' x = bundle (f x) (f' x)
  primal f = primal ∘ f
  tangent f = tangent ∘ f
  vzero f = vzero ∘ f

instance TVB a a' ta ⇒ TVB [a] [a'] [ta] where -- lengths should also be equal
  bundle = zipWith bundle
  primal = fmap primal
  tangent = fmap tangent
  vzero = fmap vzero
  vlift = fmap vlift

-- Need to define converters DA to/from TVB, in order to implement DG
-- via DA.

class ConvertTVBandDA a a' ta da ba
 | a→a', a→ta, a→da, a→ba,
   ta→a, ta→a', ta→da, ta→ba,
   ba→a, ba→a', ba→da, ba→ta
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
  fromDAtoTVB f = fromDAtoTVB ∘ f ∘ lift
  fromTVBtoDA f = fromTVBtoDA ∘ f ∘ prim

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

pushforward f = fromDAtoTVB ∘ lift f ∘ fromTVBtoDA

diff ∷ (TVB a a' ta,
        DA a da ba,
        Num a',
        ConvertTVBandDA a a' ta da ba,
        DA b db bb,
        TVB b b' tb,
        ConvertTVBandDA b b' tb db bb)
       ⇒ (a→b)→(a→b')

diff f x = tangent $ pushforward f $ bundle x 1
