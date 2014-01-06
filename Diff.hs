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

class ConvertTVBandDA tag a a' ta da ba
 | tag a→a' ta da ba,
   ta→tag a,
   ba→tag a
 where
  fromTVBtoDA ∷  (TVB tag a a' ta, DA tag a da ba) ⇒ ta→ba
  fromDAtoTVB ∷  (TVB tag a a' ta, DA tag a da ba) ⇒ ba→ta

instance ConvertTVBandDA tag Double Double (Dual tag Double) Double (Dual tag Double)
 where
  fromTVBtoDA = id
  fromDAtoTVB = id

-- instance (Num a, TVB tag a a (Dual tag a), DA tag a a (Dual tag a))
--          ⇒ ConvertTVBandDA tag a a (Dual tag a) a (Dual tag a) where
--   fromTVBtoDA = id
--   fromDAtoTVB = id

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba,
          TVB tag b b' tb,
          DA tag b db bb,
          ConvertTVBandDA tag b b' tb db bb)
         ⇒
         ConvertTVBandDA tag (a→b) (a→b') (a→tb) (da→db) (ba→bb)
 where
  fromDAtoTVB f = fromDAtoTVB ∘ f ∘ DA.lift
  fromTVBtoDA f = fromTVBtoDA ∘ f ∘ DA.primal -- *unsafe* unless tangent is zero

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba,
          TVB tag b b' tb,
          DA tag b db bb,
          ConvertTVBandDA tag b b' tb db bb)
         ⇒
         ConvertTVBandDA tag (a,b) (a',b') (ta,tb) (da,db) (ba,bb)
 where
  fromDAtoTVB (x,y) = (fromDAtoTVB x, fromDAtoTVB y)
  fromTVBtoDA (x,y) = (fromTVBtoDA x, fromTVBtoDA y)

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba)
         ⇒
         ConvertTVBandDA tag [a] [a'] [ta] [da] [ba]
 where
  fromDAtoTVB = fmap fromDAtoTVB
  fromTVBtoDA = fmap fromTVBtoDA

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba)
         ⇒
         ConvertTVBandDA tag (Maybe a) (Maybe a') (Maybe ta) (Maybe da) (Maybe ba)
 where
  fromDAtoTVB = fmap fromDAtoTVB
  fromTVBtoDA = fmap fromTVBtoDA

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba,
          TVB tag b b' tb,
          DA tag b db bb,
          ConvertTVBandDA tag b b' tb db bb)
         ⇒
         ConvertTVBandDA tag (Either a b) (Either a' b') (Either ta tb) (Either da db) (Either ba bb)
 where
  fromDAtoTVB (Left  da) = Left  (fromDAtoTVB da)
  fromDAtoTVB (Right db) = Right (fromDAtoTVB db)
  fromTVBtoDA (Left  ta) = Left  (fromTVBtoDA ta)
  fromTVBtoDA (Right tb) = Right (fromTVBtoDA tb)

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

pushforward f = fromDAtoTVB ∘ DA.lift f ∘ fromTVBtoDA

diff ∷ (TVB tag a a' ta,
        DA tag a da ba,
        Num a',
        ConvertTVBandDA tag a a' ta da ba,
        DA tag b db bb,
        TVB tag b b' tb,
        ConvertTVBandDA tag b b' tb db bb)
       ⇒ (a→b)→(a→b')

diff f = TVB.tangent ∘ pushforward f ∘ flip TVB.bundle 1
