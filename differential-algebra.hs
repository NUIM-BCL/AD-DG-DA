{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude.Unicode

-- Differential Algebra domain
class DA a b c | c→a, c→b, a→b, b→a, a→c, b→c where
  bund ∷ a→b→c
  unbund ∷ c→(a,b)            -- inverse uncurried bund
  unbund x = (prim x, tang x)
  prim ∷ c→a
  prim = fst ∘ unbund
  tang ∷ c→b
  tang = snd ∘ unbund
  zero ∷ b
  lift ∷ a→c
  lift = flip bund zero

data Dual a = Dual a a
            deriving (Read, Show)

instance Eq a ⇒ Eq (Dual a) where
  Dual x _ == Dual y _ = x ≡ y -- Can use ≡ on RHS but must use == on LHS.

instance Ord a ⇒ Ord (Dual a) where
  compare (Dual x _) (Dual y _) = compare x y

-- instance Num a ⇒ DA a a (Dual a) where
--   bund = Dual
--   unbund (Dual x x') = (x,x')
--   zero = 0

instance DA Double Double (Dual Double) where
  bund = Dual
  unbund (Dual x x') = (x,x')
  zero = 0

instance (DA a b c, DA aa bb cc) ⇒ DA (a→aa) (b→bb) (c→cc) where
  bund = error "bund not implemented for function type"
  unbund = error "unbund not implemented for function type"
  prim = (prim ∘)∘(∘ lift)
  tang = error "tang not implemented for function type"
  zero = const zero
  lift = error "lift not implemented for function type" -- isn't this just id?

instance (DA a b c, DA aa bb cc) ⇒ DA (a,aa) (b,bb) (c,cc) where
  bund (x,xx) (y,yy) = (bund x y, bund xx yy)
  unbund (x, xx) = ((prim x, prim xx), (tang x, tang xx))
  zero = (zero, zero)

{-
instance (DA a b c, Functor f) ⇒ DA (f a) (f b) (f c) where
  -- prim = fmap prim
  -- tang = fmap tang
  -- lift = fmap lift
  -- unbund fx = (fmap fst px, fmap snd px)
  --   where px = fmap unbund fx
-}

instance DA a b c ⇒ DA [a] [b] [c] where
  bund = zipWith bund
  unbund xs = (map prim xs, map tang xs)
  zero = repeat zero

-- Tangent vector bundle
class TVB a b c | c→a, c→b, a→b, b→a, a→c, b→c where
  bundle ∷ a→b→c
  unbundle ∷ c→(a,b)
  unbundle x = (primal x, tangent x)
  primal ∷ c→a
  primal = fst ∘ unbundle
  tangent ∷ c→b
  tangent = snd ∘ unbundle
  vzero ∷ b
  vlift ∷ a→c
  vlift = flip bundle vzero

instance TVB Double Double (Dual Double) where
  bundle = Dual
  primal (Dual x _) = x
  tangent (Dual _ x') = x'
  vzero = 0

-- Differential Geometric (DG) definition of the tangent vector bundle
-- of a function type.
instance TVB aa bb cc ⇒ TVB (a→aa) (a→bb) (a→cc) where
  bundle f f' x = bundle (f x) (f' x)
  primal f = primal ∘ f
  tangent f = tangent ∘ f
  vzero = const vzero

{-

pushforward ∷ (TVB a a' ta, TVB b b' tb --, DA a aa aaa, DA b bb bbb
              )
              ⇒ (a→b)→(ta→tb)
pushforward f = fromDAtoTVB ∘ lift f ∘ fromTVBtoDA
-- pushforward = error "not yet quacking"

diff ∷ TVB a b c ⇒ (Double → a) → (Double → c)
-- diff f x = pushforward f (bundle x 1)
diff = error "no"

-- Need to define converters from DA to TVB, etc, in order to
-- implement DG via DA.

fromTVBtoDA ∷ (TVB a b c, DA a bb cc) ⇒ c→cc
fromTVBtoDA = error "no"
fromDAtoTVB ∷ (TVB a b c, DA a bb cc) ⇒ cc→c
fromDAtoTVB = error "no"
-}
