{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module TangentBundle (TB, bundle, unbundle, primal, tangent, zero, lift)
where

import Prelude.Unicode

import Numeric.Dual (Dual)
import qualified Numeric.Dual as Dual (bundle, unbundle, primal, tangent, zero, lift)

-- Tangent bundle
class TB a a' ta | a→ta, ta→a a' where
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

-- instance Num a ⇒ TB a a (Dual a) where
--   bundle	= Dual.bundle
--   unbundle	= Dual.unbundle
--   primal	= Dual.primal
--   tangent	= Dual.tangent
--   lift		= Dual.lift
--   zero		= Dual.zero

instance TB Double Double (Dual Double) where
  bundle	= Dual.bundle
  unbundle	= Dual.unbundle
  primal	= Dual.primal
  tangent	= Dual.tangent
  lift		= Dual.lift
  zero		= Dual.zero

instance Num a ⇒ TB (Dual a) (Dual a) (Dual (Dual a)) where
  bundle	= Dual.bundle
  unbundle	= Dual.unbundle
  primal	= Dual.primal
  tangent	= Dual.tangent
  zero		= Dual.zero
  lift		= Dual.lift

-- Differential Geometric (DG) definition of the tangent bundle
-- of a function type.
instance TB b b' tb ⇒ TB (a→b) (a→b') (a→tb) where
  bundle f f' x = bundle (f x) (f' x)
  primal f = primal ∘ f
  tangent f = tangent ∘ f
  zero f = zero ∘ f
  lift f = lift ∘ f

instance TB a a' ta ⇒ TB [a] [a'] [ta] where -- lengths should also be equal
  bundle = zipWith bundle
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero
  lift = fmap lift

instance TB a a' ta ⇒ TB (Maybe a) (Maybe a') (Maybe ta) where
  bundle (Just x) (Just dx) = Just (bundle x dx)
  bundle Nothing Nothing = Nothing
  bundle _ _ = error "nonconformant bundle"
  unbundle Nothing = (Nothing,Nothing)
  unbundle (Just bx) = (Just p, Just t) where (p,t) = unbundle bx
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero
  lift = fmap lift

instance (TB a a' ta, TB b b' tb) ⇒ TB (a,b) (a',b') (ta,tb) where
  bundle (x,y) (x',y') = (bundle x x', bundle y y')
  unbundle (tx,ty) = ((x,y),(x',y')) where
    (x,x') = unbundle tx
    (y,y') = unbundle ty
  primal (tx,ty) = (primal tx, primal ty)
  tangent (tx,ty) = (tangent tx, tangent ty)
  zero (x,y) = (zero x, zero y)
  lift (x,y) = (lift x, lift y)

instance (TB a a' ta, TB b b' tb) ⇒ TB (Either a b) (Either a' b') (Either ta tb) where
  bundle (Left x) (Left dx) = Left (bundle x dx)
  bundle (Right x) (Right dx) = Right (bundle x dx)
  bundle _ _ = error "nonconformant bundle"
  unbundle (Left bx) = (Left p, Left t) where (p,t) = unbundle bx
  unbundle (Right bx) = (Right p, Right t) where (p,t) = unbundle bx
  primal = either (Left ∘ primal) (Right ∘ primal)
  tangent = either (Left ∘ tangent) (Right ∘ tangent)
  zero = either (Left ∘ zero) (Right ∘ zero)
  lift = either (Left ∘ lift) (Right ∘ lift)

instance TB Bool () Bool where
  bundle = const
  unbundle = (,())
  primal = id
  tangent = const ()
  zero = const ()
  lift = id

instance TB () () () where
  bundle = const
  unbundle = (,())
  primal = id
  tangent = const ()
  zero = const ()
  lift = id
