> {-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Linear.Metric (norm)

A4 paper size, with 1mm = 1 unit

> a4 :: Path V2 Double
> a4 = rect (210 :: Double) (297 :: Double)

This V e ~ V2 thing is an "equational constraint", used when we need to insist
that a pair of associated type synonyms (types defined in typeclass instances)
are equal. This is enabled by the TypeFamilies extension. V e is the vector
space over which e is defined.

> hrules = mconcat [hrule 1, hrule 2, hrule 3]

> rules :: (Enveloped e, V e ~ V2, V t ~ V2, N e ~ n, N t ~ n, RealFrac n) => e -> V2 n -> t
> rules envd direction = let
>   lineDir = perp direction
>   toStart = envelopeV lineDir envd
>   toEnd = envelopeV (negated lineDir) envd
>   directionLength = norm direction
>   firstExt = envelopeV (negated direction) envd
>   lastExt = envelopeV direction envd
>   beforeCount = floor (norm firstExt / directionLength) :: Int
>   startCount = negate beforeCount :: Int
>   endCount = floor (norm lastExt / directionLength) :: Int
>   rule n = let
>     location = fromIntegral n *^ direction
>     in fromOffsets [location + toStart, location + toEnd]
>   in mconcat $ map rule [startCount..endCount]

> main = mainWith (circle 1 :: Diagram B)
