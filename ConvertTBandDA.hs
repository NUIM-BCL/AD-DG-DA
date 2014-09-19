{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ConvertTBandDA (ConvertTBandDA, toDA, toTB)
where

import Prelude.Unicode

import DifferentialAlgebra (DA)
import qualified DifferentialAlgebra as DA (lift, primal)
import TangentBundle (TB)
import Numeric.Dual (Dual)

-- type ℝ = Double

-- This is not (==).
-- (⩵)∷Eq a⇒a→a→Bool
-- (⩵)=(≡)

-- Need to define converters DA to/from TB, in order to implement DG
-- via DA.

class (TB a a' ta, DA a da ba) ⇒ ConvertTBandDA a a' ta da ba
 where
   toDA  ∷ ta → ba
   toTB ∷ ba → ta

instance (Num a, DA a a (Dual a), TB a a (Dual a)) ⇒ ConvertTBandDA a a (Dual a) a (Dual a)
 where
  toDA  = id
  toTB = id

instance (ConvertTBandDA a a' ta da ba,
          ConvertTBandDA b b' tb db bb)
         ⇒
         ConvertTBandDA (a→b) (a→b') (a→tb) (ba→db) (ba→bb)
 where
  toTB f = toTB ∘ f ∘ DA.lift
  toDA  f = toDA  ∘ f ∘ DA.primal -- *unsafe* unless DA tangent is zero

instance (ConvertTBandDA a a' ta da ba,
          ConvertTBandDA b b' tb db bb)
         ⇒
         ConvertTBandDA (a,b) (a',b') (ta,tb) (da,db) (ba,bb)
 where
  toTB (x,y) = (toTB x, toTB y)
  toDA  (x,y) = (toDA x,  toDA y)

instance (ConvertTBandDA a a' ta da ba)
         ⇒
         ConvertTBandDA [a] [a'] [ta] [da] [ba]
 where
  toTB = fmap toTB
  toDA  = fmap toDA

instance (ConvertTBandDA a a' ta da ba)
         ⇒
         ConvertTBandDA (Maybe a) (Maybe a') (Maybe ta) (Maybe da) (Maybe ba)
 where
  toTB = fmap toTB
  toDA  = fmap toDA

instance (ConvertTBandDA a a' ta da ba,
          ConvertTBandDA b b' tb db bb)
         ⇒
         ConvertTBandDA (Either a b)
         (Either a' b') (Either ta tb)
         (Either da db) (Either ba bb)
 where
  toTB (Left  da) = Left  (toTB da)
  toTB (Right db) = Right (toTB db)
  toDA  (Left  ta) = Left  (toDA ta)
  toDA  (Right tb) = Right (toDA tb)

instance ConvertTBandDA Bool () Bool () Bool
 where
  toTB = id
  toDA  = id

instance ConvertTBandDA () () () () ()
 where
  toTB = id
  toDA  = id
