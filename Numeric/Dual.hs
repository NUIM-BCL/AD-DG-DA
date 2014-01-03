{-# LANGUAGE UnicodeSyntax #-}

{- Minimally Fart around with Dual Numbers -}

module Numeric.Dual -- (Dual)
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

lift ∷ Num a ⇒ a → Dual tag a
lift = flip Dual 0

instance Eq a ⇒ Eq (Dual tag a) where
  Dual x _ == Dual y _ = x ≡ y -- Can use ≡ on RHS but must use == on LHS.

instance Ord a ⇒ Ord (Dual tag a) where
  compare (Dual x _) (Dual y _) = compare x y

instance Num a ⇒ Num (Dual tag a) where
  Dual x x' + Dual y y' = Dual (x+y) (x'+y')
  Dual x x' * Dual y y' = Dual (x⋅y) (x⋅y'+x'⋅y) -- Can use ⋅ on RHS but must use * on LHS.
  signum (Dual x _) = lift $ signum x
  -- signum (Dual x x') = Dual (signum x) (x'⋅0)
  abs x = x ⋅ signum x
  fromInteger = lift ∘ fromInteger

instance Fractional a ⇒ Fractional (Dual tag a) where
  recip (Dual x x') = Dual (recip x) (-x'/x^(2∷Int))
  fromRational = lift ∘ fromRational
