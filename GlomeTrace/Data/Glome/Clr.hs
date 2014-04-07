module Data.Glome.Clr where

import Data.List(foldl')

type CFlt = Double
data Color = Color !CFlt !CFlt !CFlt  deriving Show
data ColorA = ColorA !CFlt !CFlt !CFlt !CFlt 

c_black = Color 0 0 0
c_white = Color 1 1 1
c_red   = Color 1 0 0
c_green = Color 0 1 0
c_blue  = Color 0 0 1

ca_black = ColorA 0 0 0 1
ca_white = ColorA 1 1 1 1
ca_red   = ColorA 1 0 0 1
ca_green = ColorA 0 1 0 1
ca_blue  = ColorA 0 0 1 1

ca_transparent = ColorA 0 0 0 0

cadd :: Color -> Color -> Color
cadd (Color r1 g1 b1) (Color r2 g2 b2) =
 Color (r1+r2) (g1+g2) (b1+b2)

caadd :: ColorA -> ColorA -> ColorA
caadd (ColorA r1 g1 b1 a1) (ColorA r2 g2 b2 a2) =
 ColorA (r1*a1 + r2*a2) (g1*a1 + g2*a2) (b1*a1 + b2*a2) (a1+a2)

canorm :: ColorA -> ColorA
canorm c@(ColorA r g b a)
  | a <= 1 = c
  | otherwise = ColorA (r/a) (g/a) (b/a) 1

cdiv :: Color -> CFlt -> Color
cdiv c1 div =
 cscale c1 (1/div)

cadiv :: ColorA -> CFlt -> ColorA
cadiv (ColorA r g b a) d =
 ColorA (r/d) (g/d) (b/d) (a/d)

cscale :: Color -> CFlt -> Color
cscale (Color r g b) mul =
 Color (r * mul) (g * mul) (b * mul)

cascale :: ColorA -> CFlt -> ColorA
cascale (ColorA r g b a) mul =
 ColorA (r * mul) (g * mul) (b*mul) a

cmul :: Color -> Color -> Color
cmul (Color r1 g1 b1) (Color r2 g2 b2) =
 Color (r1*r2) (g1*g2) (b1*b2)

cavg :: Color -> Color -> Color
cavg c1 c2 = cscale (cadd c1 c2) 0.5

cscaleadd :: Color -> Color -> CFlt -> Color
cscaleadd (Color r1 g1 b1) (Color r2 g2 b2) mul =
 Color (r1+(r2*mul)) (g1+(g2*mul)) (b1+(b2*mul))

cclamp :: Color -> Color
cclamp (Color r g b) = 
 Color (if r > 0.0 then r else 0.0)
       (if g > 0.0 then g else 0.0)
       (if b > 0.0 then b else 0.0)

color r g b = Color r g b
colora r g b a = ColorA r g b a

liftcolor :: Color -> ColorA
liftcolor (Color r g b) = ColorA r g b 1

aclamp :: CFlt -> CFlt
aclamp x
  | x > 1 = 1
  | x < 0 = 0
  | otherwise = x

-- return the final transparency after going through multiple alpha channels
alphas :: [ColorA] -> CFlt
alphas cs =
  let as = map (\(ColorA _ _ _ a) -> 1 - (aclamp a)) cs
  in 1 - (product as)

caweight :: ColorA -> ColorA -> CFlt -> ColorA
caweight (ColorA r1 g1 b1 a1) (ColorA r2 g2 b2 a2) weight =
  ColorA (w r1 r2) (w g1 g2) (w b1 b2) (w a1 a2)
  where
   w a b = (a * weight) + (b * (1-weight))

casum :: [ColorA] -> ColorA
casum cs =
  let Color r g b = foldl'
                      (\(Color r1 g1 b1) (ColorA r2 g2 b2 a2) ->
                         Color (r1 + r2*a2) (g1 + g2*a2) (b1 + b2*a2)
                      )
                      c_black
                      cs
      a = alphas cs
  in
     ColorA r g b a

-- combine layered colors, where the top layer hides the lower layers
cafold :: ColorA -> ColorA -> ColorA
cafold (ColorA r1 g1 b1 a1) (ColorA r2 g2 b2 a2) =
  ColorA (r1 + (r2 * trans * a2))
         (g1 + (g2 * trans * a2))
         (b1 + (b2 * trans * a2))
         (a1 + (a2 * trans))
  where
    trans = 1-a1


