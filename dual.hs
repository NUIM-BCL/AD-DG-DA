{-# LANGUAGE UnicodeSyntax #-}
import Prelude.Unicode

{- Minimally Fart around with Dual Numbers -}

data Dual a = Dual a a
            deriving (Ord, Show, Eq)

instance Num a ⇒ Num (Dual a) where
  Dual x x' + Dual y y' = Dual (x+y) (x'+y')
  Dual x x' * Dual y y' = Dual (x⋅y) (x⋅y'+x'⋅y)
  signum (Dual x x') = Dual (signum x) (x'⋅0)
  abs x = x ⋅ signum x
  fromInteger = flip Dual 0 ∘ fromInteger

instance Fractional a ⇒ Fractional (Dual a) where
  recip (Dual x x') = Dual (recip x) (-x'/x^(2∷Int))
  fromRational = flip Dual 0 ∘ fromRational
