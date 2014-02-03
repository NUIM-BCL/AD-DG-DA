{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module TangentVectorBundle (TVB, bundle, unbundle, primal, tangent, zero, lift)
where

import Prelude.Unicode

import Numeric.Dual (Dual)
import qualified Numeric.Dual as Dual (lift, bundle, unbundle, primal, tangent)

-- Tangent vector bundle
class TVB tag a a' ta | tag a→ta, ta→tag a a', a'→tag where -- a'→tag is suspect
  bundle ∷ a→a'→ta
  unbundle ∷ ta→(a,a')
  unbundle x = (primal x, tangent x)
  primal ∷ ta→a
  primal = fst ∘ unbundle
  tangent ∷ ta→a'
  tangent = snd ∘ unbundle
  zero ∷ a→a'
  lift ∷ a→ta
  lift x = bundle x (zero x)

-- instance Num a ⇒ TVB tag a a (Dual tag a) where
--   bundle	= Dual.bundle
--   unbundle	= Dual.unbundle
--   primal	= Dual.primal
--   tangent	= Dual.tangent
--   lift		= Dual.lift
--   zero		= const 0

instance TVB tag Double Double (Dual tag Double) where
  bundle	= Dual.bundle
  unbundle	= Dual.unbundle
  primal	= Dual.primal
  tangent	= Dual.tangent
  lift		= Dual.lift
  zero		= const 0

-- Differential Geometric (DG) definition of the tangent vector bundle
-- of a function type.
instance TVB tag b b' tb ⇒ TVB tag (a→b) (a→b') (a→tb) where
  bundle f f' x = bundle (f x) (f' x)
  primal f = primal ∘ f
  tangent f = tangent ∘ f
  zero f = zero ∘ f

instance TVB tag a a' ta ⇒ TVB tag [a] [a'] [ta] where -- lengths should also be equal
  bundle = zipWith bundle
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero
  lift = fmap lift

instance TVB tag a a' ta ⇒ TVB tag (Maybe a) (Maybe a') (Maybe ta) where
  bundle (Just x) (Just dx) = Just (bundle x dx)
  bundle Nothing Nothing = Nothing
  bundle _ _ = error "nonconformant bundle"
  unbundle Nothing = (Nothing,Nothing)
  unbundle (Just bx) = (Just p, Just t) where (p,t) = unbundle bx
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero
  lift = fmap lift

instance (TVB tag a a' ta, TVB tag b b' tb) ⇒ TVB tag (a,b) (a',b') (ta,tb) where
  bundle (x,y) (x',y') = (bundle x x', bundle y y')
  unbundle (tx,ty) = ((x,y),(x',y')) where
    (x,x') = unbundle tx
    (y,y') = unbundle ty
  primal (tx,ty) = (primal tx, primal ty)
  tangent (tx,ty) = (tangent tx, tangent ty)
  zero (x,y) = (zero x, zero y)
  lift (x,y) = (lift x, lift y)

instance (TVB tag a a' ta, TVB tag b b' tb) ⇒ TVB tag (Either a b) (Either a' b') (Either ta tb) where
  bundle (Left x) (Left dx) = Left (bundle x dx)
  bundle (Right x) (Right dx) = Right (bundle x dx)
  bundle _ _ = error "nonconformant bund"
  unbundle (Left bx) = (Left p, Left t) where (p,t) = unbundle bx
  unbundle (Right bx) = (Right p, Right t) where (p,t) = unbundle bx
  primal = either (Left ∘ primal) (Right ∘ primal)
  tangent = either (Left ∘ tangent) (Right ∘ tangent)
  zero = either (Left ∘ zero) (Right ∘ zero)
  lift = either (Left ∘ lift) (Right ∘ lift)
