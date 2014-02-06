{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module DifferentialAlgebra (DA, bundle, unbundle, primal, tangent, zero, lift)
where

import Prelude.Unicode

import Numeric.Dual ({- DualNumber, -} Dual)
import qualified Numeric.Dual as Dual (bundle, unbundle, primal, tangent, zero, lift)

-- | Differential Algebra domain.
-- The type class (DA t dt bt) means -- that t is the type of primal
-- values, dt is the type of the "tangents", and bt is the bundle type
-- holding both t and dt.
class DA a da ba | a→ba, ba→a da where
  bundle ∷ a→da→ba
  unbundle ∷ ba→(a,da)            -- inverse uncurried bundle
  unbundle x = (primal x, tangent x)
  primal ∷ ba→a
  primal = fst ∘ unbundle
  tangent ∷ ba→da
  tangent = snd ∘ unbundle
  zero ∷ a→da
  lift ∷ a→ba
  lift x = bundle x (zero x)

-- instance Num a ⇒ DA a a (Dual a) where
--   bundle	= Dual.bundle
--   unbundle	= Dual.unbundle
--   primal	= Dual.primal
--   tangent	= Dual.tangent
--   zero		= Dual.zero
--   lift		= Dual.lift

instance DA Double Double (Dual Double) where
  bundle	= Dual.bundle
  unbundle	= Dual.unbundle
  primal	= Dual.primal
  tangent	= Dual.tangent
  zero		= Dual.zero
  lift		= Dual.lift

-- It is unclear what the 2nd arg to DA should be here.  The (ba→db) is
-- just a placeholder.  The "right thing" is probably to get rid of
-- the 2nd arg of DA, and make another type class for DA-with-tangent.
-- Which happens to be exactly those cases where
-- (TVB a a' ta,  DA a da ba,  a'~da,  ta~ba)
instance (DA a da ba, DA b db bb) ⇒ DA (a→b) (ba→db) (ba→bb) where
  bundle f df bx = bundle (f (primal bx)) (df bx) -- WARNING, INEFFICIENT!
  primal = (primal ∘)∘(∘ lift)
  tangent = (tangent ∘)
  zero f = zero ∘ f ∘ primal    -- Dubious
  lift = error "lift not implemented for function type" -- lift = id ?

instance (DA a da ba, DA b db bb) ⇒ DA (a,b) (da,db) (ba,bb) where
  bundle (x,xx) (y,yy) = (bundle x y, bundle xx yy)
  unbundle (x, xx) = ((primal x, primal xx), (tangent x, tangent xx))
  zero (x,y) = (zero x, zero y)

{-
instance (DA a da ba, Functor f) ⇒ DA (f a) (f da) (f ba) where
  -- primal = fmap primal
  -- tangent = fmap tangent
  -- lift = fmap lift
  -- unbundle fx = (fmap fst px, fmap snd px) where px = fmap unbundle fx
-}

instance DA a da ba ⇒ DA [a] [da] [ba] where -- lengths should also be equal
  bundle = zipWith bundle
  unbundle bxs = (fmap fst us, fmap snd us) where us = fmap unbundle bxs
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero

instance DA a da ba ⇒ DA (Maybe a) (Maybe da) (Maybe ba) where
  bundle (Just x) (Just dx) = Just (bundle x dx)
  bundle Nothing Nothing = Nothing
  bundle _ _ = error "nonconformant bundle"
  unbundle Nothing = (Nothing,Nothing)
  unbundle (Just bx) = (Just p, Just t) where (p,t) = unbundle bx
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero
  lift = fmap lift

instance (DA a da ba, DA b db bb) ⇒ DA (Either a b) (Either da db) (Either ba bb) where
  bundle (Left x) (Left dx) = Left (bundle x dx)
  bundle (Right x) (Right dx) = Right (bundle x dx)
  bundle _ _ = error "nonconformant bundle"
  unbundle (Left bx) = (Left p, Left t) where (p,t) = unbundle bx
  unbundle (Right bx) = (Right p, Right t) where (p,t) = unbundle bx
  primal = either (Left ∘ primal) (Right ∘ primal)
  tangent = either (Left ∘ tangent) (Right ∘ tangent)
  zero = either (Left ∘ zero) (Right ∘ zero)
  lift = either (Left ∘ lift) (Right ∘ lift)

instance DA Bool () Bool where
  bundle = const
  unbundle = (,())
  primal = id
  tangent = const ()
  zero = const ()
  lift = id

instance DA () () () where
  bundle = const
  unbundle = (,())
  primal = id
  tangent = const ()
  zero = const ()
  lift = id
