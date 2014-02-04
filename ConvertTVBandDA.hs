{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module ConvertTVBandDA (ConvertTVBandDA, toDA, toTVB)
where

import Prelude.Unicode

import DifferentialAlgebra (DA)
import qualified DifferentialAlgebra as DA (lift, primal)
import TangentVectorBundle (TVB)
import Numeric.Dual (Dual)

-- type ℝ = Double

-- This is not (==).
-- (⩵)∷Eq a⇒a→a→Bool
-- (⩵)=(≡)

-- Need to define converters DA to/from TVB, in order to implement DG
-- via DA.

class ConvertTVBandDA a a' ta da ba
 | a→a' ta da ba,
   ta→a,
   ba→a
 where
   toDA  ∷ (TVB a a' ta, DA a da ba) ⇒ ta→ba
   toTVB ∷ (TVB a a' ta, DA a da ba) ⇒ ba→ta

instance ConvertTVBandDA Double Double (Dual Double) Double (Dual Double)
 where
   toDA  = id
   toTVB = id

-- instance (Num a, TVB a a (Dual a), DA a a (Dual a))
--          ⇒ ConvertTVBandDA a a (Dual a) a (Dual a) where
--   toDA  = id
--   toTVB = id

instance (TVB a a' ta,
          DA a da ba,
          ConvertTVBandDA a a' ta da ba,
          TVB b b' tb,
          DA b db bb,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (a→b) (a→b') (a→tb) (ba→db) (ba→bb)
 where
  toTVB f = toTVB ∘ f ∘ DA.lift
  toDA  f = toDA  ∘ f ∘ DA.primal -- *unsafe* unless DA tangent is zero

instance (TVB a a' ta,
          DA a da ba,
          ConvertTVBandDA a a' ta da ba,
          TVB b b' tb,
          DA b db bb,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (a,b) (a',b') (ta,tb) (da,db) (ba,bb)
 where
  toTVB (x,y) = (toTVB x, toTVB y)
  toDA (x,y) = (toDA x, toDA y)

instance (TVB a a' ta,
          DA a da ba,
          ConvertTVBandDA a a' ta da ba)
         ⇒
         ConvertTVBandDA [a] [a'] [ta] [da] [ba]
 where
  toTVB = fmap toTVB
  toDA = fmap toDA

instance (TVB a a' ta,
          DA a da ba,
          ConvertTVBandDA a a' ta da ba)
         ⇒
         ConvertTVBandDA (Maybe a) (Maybe a') (Maybe ta) (Maybe da) (Maybe ba)
 where
  toTVB = fmap toTVB
  toDA = fmap toDA

instance (TVB a a' ta,
          DA a da ba,
          ConvertTVBandDA a a' ta da ba,
          TVB b b' tb,
          DA b db bb,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (Either a b)
         (Either a' b') (Either ta tb)
         (Either da db) (Either ba bb)
 where
  toTVB (Left  da) = Left  (toTVB da)
  toTVB (Right db) = Right (toTVB db)
  toDA (Left  ta) = Left  (toDA ta)
  toDA (Right tb) = Right (toDA tb)
