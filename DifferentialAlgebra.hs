{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module DifferentialAlgebra (diff, pushforward,
                            ConvertTVBandDA, TVB, DA,
                            bund, unbund, prim, tang, zero, lift,
                            bundle, unbundle, primal, tangent, vzero, vlift)
where

import Prelude.Unicode

import Numeric.Dual hiding (lift)
import qualified Numeric.Dual as Dual()

-- | Differential Algebra domain.
-- The type class (DA t dt bt) means -- that t is the type of primal
-- values, dt is the type of the "tangents", and bt is the bundle type
-- holding both t and dt.
class DA a da ba | ba→a, ba→da, a→da, a→ba where
  bund ∷ a→da→ba
  unbund ∷ ba→(a,da)            -- inverse uncurried bund
  unbund x = (prim x, tang x)
  prim ∷ ba→a
  prim = fst ∘ unbund
  tang ∷ ba→da
  tang = snd ∘ unbund
  zero ∷ a→da
  lift ∷ a→ba
  lift x = bund x (zero x)

-- instance Num a ⇒ DA a a (Dual tag a) where
--   bund = Dual
--   unbund (Dual x x') = (x,x')
--   zero = const 0

instance DA Double Double (Dual tag Double) where
  bund = Dual
  unbund (Dual x x') = (x,x')
  zero = const 0

-- It is unclear what the 2nd arg to DA should be here.  The (ba→db) is
-- just a placeholder.  The "right thing" is probably to get rid of
-- the 2nd arg of DA, and make another type class for DA-with-tang.
-- Which happens to be exactly those cases where
-- (TVB a a' ta, DA a da ba, a'~da, ta~ba)
instance (DA a da ba, DA b db bb) ⇒ DA (a→b) (ba→db) (ba→bb) where
  bund = error "bund not implemented for function type"
  unbund = error "unbund not implemented for function type"
  prim = (prim ∘)∘(∘ lift)
  tang = error "tang not implemented for function type"
  zero = error "zero not implemented for function type"
  lift = error "lift not implemented for function type" -- lift = id ?

instance (DA a da ba, DA b db bb) ⇒ DA (a,b) (da,db) (ba,bb) where
  bund (x,xx) (y,yy) = (bund x y, bund xx yy)
  unbund (x, xx) = ((prim x, prim xx), (tang x, tang xx))
  zero (x,y) = (zero x, zero y)

{-
instance (DA a da ba, Functor f) ⇒ DA (f a) (f da) (f ba) where
  -- prim = fmap prim
  -- tang = fmap tang
  -- lift = fmap lift
  -- unbund fx = (fmap fst px, fmap snd px)
  --   where px = fmap unbund fx
-}

instance DA a da ba ⇒ DA [a] [da] [ba] where -- lengths should also be equal
  bund = zipWith bund
  unbund xs = (fmap prim xs, fmap tang xs)
  zero = fmap zero

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
