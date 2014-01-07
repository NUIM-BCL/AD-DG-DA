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


-- Need to define converters DA to/from TVB, in order to implement DG
-- via DA.

class ConvertTVBandDA tag a a' ta da ba
 | tag a→a' ta da ba,
   ta→tag a,
   ba→tag a
 where
   toDA  ∷ (TVB tag a a' ta, DA tag a da ba) ⇒ ta→ba
   toTVB ∷ (TVB tag a a' ta, DA tag a da ba) ⇒ ba→ta

instance ConvertTVBandDA tag Double Double (Dual tag Double) Double (Dual tag Double)
 where
   toDA  = id
   toTVB = id

-- instance (Num a, TVB tag a a (Dual tag a), DA tag a a (Dual tag a))
--          ⇒ ConvertTVBandDA tag a a (Dual tag a) a (Dual tag a) where
--   toDA  = id
--   toTVB = id

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba,
          TVB tag b b' tb,
          DA tag b db bb,
          ConvertTVBandDA tag b b' tb db bb)
         ⇒
         ConvertTVBandDA tag (a→b) (a→b') (a→tb) (ba→db) (ba→bb)
 where
  toTVB f = toTVB ∘ f ∘ DA.lift
  toDA  f = toDA  ∘ f ∘ DA.primal -- *unsafe* unless DA tangent is zero

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba,
          TVB tag b b' tb,
          DA tag b db bb,
          ConvertTVBandDA tag b b' tb db bb)
         ⇒
         ConvertTVBandDA tag (a,b) (a',b') (ta,tb) (da,db) (ba,bb)
 where
  toTVB (x,y) = (toTVB x, toTVB y)
  toDA (x,y) = (toDA x, toDA y)

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba)
         ⇒
         ConvertTVBandDA tag [a] [a'] [ta] [da] [ba]
 where
  toTVB = fmap toTVB
  toDA = fmap toDA

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba)
         ⇒
         ConvertTVBandDA tag (Maybe a) (Maybe a') (Maybe ta) (Maybe da) (Maybe ba)
 where
  toTVB = fmap toTVB
  toDA = fmap toDA

instance (TVB tag a a' ta,
          DA tag a da ba,
          ConvertTVBandDA tag a a' ta da ba,
          TVB tag b b' tb,
          DA tag b db bb,
          ConvertTVBandDA tag b b' tb db bb)
         ⇒
         ConvertTVBandDA tag (Either a b)
         (Either a' b') (Either ta tb)
         (Either da db) (Either ba bb)
 where
  toTVB (Left  da) = Left  (toTVB da)
  toTVB (Right db) = Right (toTVB db)
  toDA (Left  ta) = Left  (toDA ta)
  toDA (Right tb) = Right (toDA tb)
