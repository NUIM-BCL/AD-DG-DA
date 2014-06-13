Get the type of AD right in Haskell.

Forward accumulation mode automatic differentiation only, for now.
(There is no barrier in principle to extending it to cover reverse
mode.)

This cannot be used to actually take derivatives, because
````DA.lift```` is not implemented for function objects.  It seems
like this would require a reflective mechanism, which is unavailable.

There are two main mechanisms involved.  One is a "tangent vector
bundle" type class, which is the ````T```` functor from differential
geometry and has the property ````T (a->b) = a -> T b```` and ````T
Double = Dual Double````.  The other is a "differential algebra" type
class, which lifts an object to replace all ````Double````s by
````Dual Double```` inside and out, so ````D (a->b) = D a -> D b````.
The former is the "right" API for differentiation, namely the
pushforward operator.  The latter is the natural implementation
mechanism.  Goo is defined to translate between the two as needed, at
the interface between the API of differentiation and its
implementation.

The heart of the implementation then becomes the apparently
unimplementable ````DA.lift```` on functions.

But the types work.

```
$ ghci -Wall
GHCi, version 7.6.3

Prelude> :l Diff.hs
[1 of 5] Compiling Numeric.Dual     ( Numeric/Dual.hs, interpreted )
[2 of 5] Compiling TangentVectorBundle ( TangentVectorBundle.hs, interpreted )
[3 of 5] Compiling DifferentialAlgebra ( DifferentialAlgebra.hs, interpreted )
[4 of 5] Compiling ConvertTVBandDA  ( ConvertTVBandDA.hs, interpreted )
[5 of 5] Compiling Diff             ( Diff.hs, interpreted )
Ok, modules loaded: Diff, ConvertTVBandDA, DifferentialAlgebra, TangentVectorBundle, Numeric.Dual.

*Diff> :m + Numeric.Dual 

*Diff Numeric.Dual> :t diff (sin::Double->Double)

diff (sin::Double->Double) :: Double -> Double

*Diff Numeric.Dual> :t iterate diff ((*)::Double->Double->Double)

iterate diff ((*)::Double->Double->Double)
  :: [Double -> Double -> Double]

*Diff Numeric.Dual> :t pushforward ((*)::Double->Double->Double)

pushforward ((*)::Double->Double->Double)
  :: Dual Double -> Double -> Dual Double

*Diff Numeric.Dual> :t pushforward (pushforward ((*)::Double->Double->Double))

pushforward (pushforward ((*)::Double->Double->Double))
  :: Dual (Dual Double) -> Double -> Dual (Dual Double)

*Diff Numeric.Dual> :t diff (pushforward (pushforward ((*)::Double->Double->Double)))

diff (pushforward (pushforward ((*)::Double->Double->Double)))
  :: Dual (Dual Double) -> Double -> Dual (Dual Double)

*Diff Numeric.Dual> :t pushforward (flip (pushforward (pushforward ((*)::Double->Double->Double))))

pushforward (flip (pushforward (pushforward ((*)::Double->Double->Double))))
  :: Dual Double -> Dual (Dual Double) -> Dual (Dual (Dual Double))

*Diff Numeric.Dual> :t pushforward (uncurry ((*)::Double->Double->Double))

pushforward (uncurry ((*)::Double->Double->Double))
  :: (Dual Double, Dual Double) -> Dual Double

*Diff Numeric.Dual> :t pushforward (sum::[Double]->Double)

pushforward (sum::[Double]->Double) :: [Dual Double] -> Dual Double

*Diff Numeric.Dual> :t pushforward sum

pushforward sum
  :: (Num a, ConvertTVBandDA a a' ta da ba) => [ta] -> ta
````
