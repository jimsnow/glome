module Data.Glome.Clr where

type CFlt = Double
data Color = Color {r,g,b :: !CFlt} deriving Show

c_black = Color 0 0 0
c_white = Color 1 1 1
c_red   = Color 1 0 0
c_green = Color 0 1 0
c_blue  = Color 0 0 1

cadd :: Color -> Color -> Color
cadd (Color r1 g1 b1) (Color r2 g2 b2) =
 Color (r1+r2) (g1+g2) (b1+b2)

cdiv :: Color -> CFlt -> Color
cdiv c1 div =
 cscale c1 (1/div)

cscale :: Color -> CFlt -> Color
cscale (Color r g b) mul =
 Color (r * mul)
       (g * mul)
       (b * mul)

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
