{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

class (TVB a a' ta, DA a da ba) ⇒ ConvertTVBandDA a a' ta da ba
 where
   toDA  ∷ ta → ba
   toTVB ∷ ba → ta

instance (Num a, DA a a (Dual a), TVB a a (Dual a)) ⇒ ConvertTVBandDA a a (Dual a) a (Dual a)
 where
  toDA  = id
  toTVB = id

instance (ConvertTVBandDA a a' ta da ba,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (a→b) (a→b') (a→tb) (ba→db) (ba→bb)
 where
  toTVB f = toTVB ∘ f ∘ DA.lift
  toDA  f = toDA  ∘ f ∘ DA.primal -- *unsafe* unless DA tangent is zero

instance (ConvertTVBandDA a a' ta da ba,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (a,b) (a',b') (ta,tb) (da,db) (ba,bb)
 where
  toTVB (x,y) = (toTVB x, toTVB y)
  toDA  (x,y) = (toDA x,  toDA y)

instance (ConvertTVBandDA a a' ta da ba)
         ⇒
         ConvertTVBandDA [a] [a'] [ta] [da] [ba]
 where
  toTVB = fmap toTVB
  toDA  = fmap toDA

instance (ConvertTVBandDA a a' ta da ba)
         ⇒
         ConvertTVBandDA (Maybe a) (Maybe a') (Maybe ta) (Maybe da) (Maybe ba)
 where
  toTVB = fmap toTVB
  toDA  = fmap toDA

instance (ConvertTVBandDA a a' ta da ba,
          ConvertTVBandDA b b' tb db bb)
         ⇒
         ConvertTVBandDA (Either a b)
         (Either a' b') (Either ta tb)
         (Either da db) (Either ba bb)
 where
  toTVB (Left  da) = Left  (toTVB da)
  toTVB (Right db) = Right (toTVB db)
  toDA  (Left  ta) = Left  (toDA ta)
  toDA  (Right tb) = Right (toDA tb)
