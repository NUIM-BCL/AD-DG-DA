{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module DifferentialAlgebra (DA, bund, unbund, primal, tangent, zero, lift)
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
  unbund x = (primal x, tangent x)
  primal ∷ ba→a
  primal = fst ∘ unbund
  tangent ∷ ba→da
  tangent = snd ∘ unbund
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
-- the 2nd arg of DA, and make another type class for DA-with-tangent.
-- Which happens to be exactly those cases where
-- (TVB a a' ta, DA a da ba, a'~da, ta~ba)
instance (DA a da ba, DA b db bb) ⇒ DA (a→b) (ba→db) (ba→bb) where
  bund = error "bund not implemented for function type"
  unbund = error "unbund not implemented for function type"
  primal = (primal ∘)∘(∘ lift)
  tangent = error "tangent not implemented for function type"
  zero = error "zero not implemented for function type"
  lift = error "lift not implemented for function type" -- lift = id ?

instance (DA a da ba, DA b db bb) ⇒ DA (a,b) (da,db) (ba,bb) where
  bund (x,xx) (y,yy) = (bund x y, bund xx yy)
  unbund (x, xx) = ((primal x, primal xx), (tangent x, tangent xx))
  zero (x,y) = (zero x, zero y)

{-
instance (DA a da ba, Functor f) ⇒ DA (f a) (f da) (f ba) where
  -- primal = fmap primal
  -- tangent = fmap tangent
  -- lift = fmap lift
  -- unbund fx = (fmap fst px, fmap snd px) where px = fmap unbund fx
-}

instance DA a da ba ⇒ DA [a] [da] [ba] where -- lengths should also be equal
  bund = zipWith bund
  unbund bxs = (fmap fst us, fmap snd us) where us = fmap unbund bxs
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero

instance DA a da ba ⇒ DA (Maybe a) (Maybe da) (Maybe ba) where
  bund (Just x) (Just dx) = Just (bund x dx)
  bund Nothing Nothing = Nothing
  bund _ _ = error "nonconformant bund"
  unbund Nothing = (Nothing,Nothing)
  unbund (Just bx) = (Just p, Just t) where (p,t) = unbund bx
  primal = fmap primal
  tangent = fmap tangent
  zero = fmap zero
  lift = fmap lift

instance (DA a da ba, DA b db bb) ⇒ DA (Either a b) (Either da db) (Either ba bb) where
  bund (Left x) (Left dx) = Left (bund x dx)
  bund (Right x) (Right dx) = Right (bund x dx)
  bund _ _ = error "nonconformant bund"
  unbund (Left bx) = (Left p, Left t) where (p,t) = unbund bx
  unbund (Right bx) = (Right p, Right t) where (p,t) = unbund bx
  primal = either (Left ∘ primal) (Right ∘ primal)
  tangent = either (Left ∘ tangent) (Right ∘ tangent)
  zero = either (Left ∘ zero) (Right ∘ zero)
  lift = either (Left ∘ lift) (Right ∘ lift)
