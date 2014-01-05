{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module TangentVectorBundle (TVB,
                            bundle, unbundle, primal, tangent, vzero, vlift)
where

import Prelude.Unicode
import Numeric.Dual hiding (lift)
import qualified Numeric.Dual as Dual()

-- Tangent vector bundle
class TVB a a' ta | ta→a, ta→a', a→a', a→ta where
  bundle ∷ a→a'→ta
  unbundle ∷ ta→(a,a')
  unbundle x = (primal x, tangent x)
  primal ∷ ta→a
  primal = fst ∘ unbundle
  tangent ∷ ta→a'
  tangent = snd ∘ unbundle
  vzero ∷ a→a'
  vlift ∷ a→ta
  vlift x = bundle x (vzero x)

-- instance Num a ⇒ TVB a a (Dual tag a) where
--   bundle = Dual
--   unbundle (Dual x x') = (x,x')
--   vzero = const 0

instance TVB Double Double (Dual tag Double) where
  bundle = Dual
  unbundle (Dual x x') = (x,x')
  vzero = const 0

-- Differential Geometric (DG) definition of the tangent vector bundle
-- of a function type.
instance TVB b b' tb ⇒ TVB (a→b) (a→b') (a→tb) where
  bundle f f' x = bundle (f x) (f' x)
  primal f = primal ∘ f
  tangent f = tangent ∘ f
  vzero f = vzero ∘ f

instance TVB a a' ta ⇒ TVB [a] [a'] [ta] where -- lengths should also be equal
  bundle = zipWith bundle
  primal = fmap primal
  tangent = fmap tangent
  vzero = fmap vzero
  vlift = fmap vlift
