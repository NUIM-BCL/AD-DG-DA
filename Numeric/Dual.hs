{-# LANGUAGE UnicodeSyntax #-}

{- Minimally Fart around with Dual Numbers -}

module Numeric.Dual (Dual, lift, bundle, unbundle, primal, tangent,
                     liftA1, liftA1_, liftA2, liftA2_)
where

import Prelude.Unicode

-- | The 'Dual' type is a concrete representation of a Dual number,
-- meaning a number augmented with a derivatives (Clifford, 1873).
-- These can be regarded as truncated power series.  The intended use
-- is for overloading arithmetic in order perform a nonstandard
-- interpretation of numeric code, for doing forward-mode automatic
-- differeniation (Wengert, 1964).  The "tag" allows branding, to
-- avoid perturbation confusion (Siskind and Pearlmutter, 2008).
data Dual tag a = Dual a a
                deriving (Read, Show)

bundle ∷ a→a→Dual tag a
bundle = Dual

unbundle ∷ Dual tag a→(a,a)
unbundle (Dual x x') = (x,x')

primal ∷ Dual tag a→a
primal (Dual x _) = x

tangent ∷ Dual tag a→a
tangent (Dual _ x') = x'

lift ∷ Num a ⇒ a → Dual tag a
lift = flip Dual 0

liftA1 ∷ Num a ⇒ (a→a) → (a→a) → (Dual tag a→Dual tag a)
liftA1 f df (Dual x x') = Dual (f x) (x' ⋅ df x)

liftA1_ ∷ Num a ⇒ (a→a) → (a→a→a) → (Dual tag a→Dual tag a)
liftA1_ f df (Dual x x') = Dual z (x' ⋅ df z x) where
  z = f x

liftA2 ∷ Num a ⇒ (a→a→a) → (a→a→(a,a)) → (Dual tag a→Dual tag a→Dual tag a)
liftA2 f df (Dual x x') (Dual y y') = Dual z z' where
  z = f x y
  (dzdx, dzdy) = df x y
  z' = x' ⋅ dzdx + y' ⋅ dzdy

liftA2_ ∷ Num a ⇒ (a→a→a) → (a→a→a→(a,a)) → (Dual tag a→Dual tag a→Dual tag a)
liftA2_ f df (Dual x x') (Dual y y') = Dual z z' where
  z = f x y
  (dzdx, dzdy) = df z x y
  z' = x' ⋅ dzdx + y' ⋅ dzdy

instance Eq a ⇒ Eq (Dual tag a) where
  Dual x _ == Dual y _ = x ≡ y -- Can use ≡ on RHS but must use == on LHS.

instance Ord a ⇒ Ord (Dual tag a) where
  compare (Dual x _) (Dual y _) = compare x y

instance Num a ⇒ Num (Dual tag a) where
  (+)		= liftA2 (+) (\_ _ → (1,1))
  (*)		= liftA2 (⋅) (flip (,)) -- Can use ⋅ on RHS but must use * on LHS.
  signum (Dual x _) = lift $ signum x
  -- signum (Dual x x') = Dual (signum x) (x'⋅0)
  abs x		= x ⋅ signum x
  fromInteger	= lift ∘ fromInteger

instance Fractional a ⇒ Fractional (Dual tag a) where
  recip = liftA1_ recip (\z _ → - z^(2∷Int))
  fromRational	= lift ∘ fromRational

instance (Eq a, Floating a) => Floating (Dual tag a) where
  pi		= lift pi
  exp		= liftA1_ exp const
  sqrt		= liftA1_ sqrt (const ∘ recip ∘ (2⋅))
  log		= liftA1 log recip
  -- The default case has a problem when the base is zero, e.g.,
  --  diffUU (**2) 0 = NaN
  -- which is wrong.
  -- Special cases are needed to bypass avoidable division by 0 and log 0.
  -- This is handled in two parts: first we deal with a constant zero exponent,
  -- then we deal with all other constant exponents.
  (**) _ (Dual 0 0) = 1
  (**) (Dual x0 0) (Dual y0 0) = lift (x0**y0)
  (**) (Dual x0 x') (Dual y0 0) = Dual (x0**y0) (x' ⋅ y0*x0**(y0-1))
  (**) x@(Dual 0 _) y = liftA2 (**) (\x0 y0 -> (y0*x0**(y0-1), 0)) x y
  (**) x@(Dual _ 0) y = liftA2_ (**) (\z x0 _ -> (0, z*log x0)) x y
  (**) x y	= liftA2_ (**) (\z x0 y0 -> (y0*z/x0, z*log x0)) x y
  sin		= liftA1 sin cos
  cos		= liftA1 cos (negate ∘ sin)
  tan		= liftA1 tan (recip ∘ (^(2∷Int)) ∘ cos)
  asin		= liftA1 asin (recip ∘ sqrt ∘ (1-) ∘ (^(2∷Int)))
  acos		= liftA1 acos (negate ∘ recip ∘ sqrt ∘ (1-) ∘ (^(2∷Int)))
  atan		= liftA1 atan (recip ∘ (1+) ∘ (^(2∷Int)))
  sinh		= liftA1 sinh cosh
  cosh		= liftA1 cosh sinh
  asinh		= liftA1 asinh (recip ∘ sqrt ∘ (1+) ∘ (^(2∷Int)))
  acosh		= liftA1 acosh (recip ∘ sqrt ∘ (-1+) ∘ (^(2∷Int)))
  atanh		= liftA1 atanh (recip ∘ (1-) ∘ (^(2∷Int)))
