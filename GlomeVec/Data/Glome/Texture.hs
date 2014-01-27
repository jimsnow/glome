{-
module SolidTexture (square_wave, triangle_wave, sine_wave, 
                     stripe, noise, turbulence
                    ) where -}

module Data.Glome.Texture where
import Data.Glome.Vec
import Data.Array.IArray

-- INTERPOLATION FUNCTIONS --
square_wave :: Flt -> Flt
square_wave x =
 let offset = x - (fromIntegral (floor x))
 in if offset < 0.5 then 0 else 1

triangle_wave :: Flt -> Flt
triangle_wave x =
 let offset = x - (fromIntegral (floor x))
 in if offset < 0.5 
    then (offset*2)
    else (2-(offset*2))

sine_wave :: Flt -> Flt
sine_wave x = (sin (x*2*pi))*0.5 + 0.5


lump_wave :: Flt -> Flt
lump_wave x = 1 - x*x*x

-- SCALAR TEXTURE FUNCTIONS --

-- These are simple solid texture functions that take a 
-- point as argument and return a number 0 < n < 1

stripe :: Vec -> (Flt -> Flt) -> (Vec -> Flt)
stripe axis interp =
 let len = vlen axis 
 in
  (\pos -> let offset = vdot pos axis 
           in interp offset)


-- PERLIN NOISE --

-- (-6 t^5 + 15 t^4 - 10t^3 +1)
-- "realistic ray tracing 2nd edition" inconsistent 
-- on whether it should be t^5 or t^6,
-- but t^5 works and t^6 doesn't.
omega :: Flt -> Flt
omega t_ = 
 let t     = fabs t_
     tsqr  = t*t
     tcube = tsqr*t
 in (-6)*tcube*tsqr + 15*tcube*t - 10*tcube + 1

-- questionably random
phi :: Array Int Int
phi = listArray (0,11) [3,0,2,7,4,1,5,11,8,10,9,6]

grad :: Array Int Vec
grad = listArray (0,11) 
         $ filter (\x -> let l = vlen x in l < 1.5 && l > 1.1) 
                  [Vec x y z | x <- [(-1),0,1],
                               y <- [(-1),0,1],
                               z <- [(-1),0,1]] 

gamma :: Int -> Int -> Int -> Vec
gamma i j k =
 let a = phi!(mod (iabs k) 12)
     b = phi!(mod (iabs (j+a)) 12)
     c = phi!(mod (iabs (i+b)) 12)
 in grad!c

knot :: Int -> Int -> Int -> Vec -> Flt
knot i j k v =
 let Vec x y z = v
 in (omega x) * (omega y) * (omega z) * (vdot (gamma i j k) v)

intGamma :: Int -> Int -> Int
intGamma i j =
 let a = phi!(mod (iabs j) 16)
     b = phi!(mod (iabs (i+a)) 16)
 in b

turbulence :: Vec -> Int -> Flt
turbulence p 1 = fabs(noise(p))
turbulence p n =
 let newp = vscale p 0.5
     t = fabs (noise p)
 in t + (0.5 * (turbulence newp (n-1)))

noise :: Vec -> Flt 
noise (Vec x y z) =
 let i = floor x
     j = floor y
     k = floor z
     u = x-(fromIntegral i)
     v = y-(fromIntegral j)
     w = z-(fromIntegral k)
 in knot i j k             (Vec u v w) +
    knot (i+1) j k         (Vec (u-1) v w) +
    knot i (j+1) k         (Vec u (v-1) w) +
    knot i j (k+1)         (Vec u v (w-1)) +
    knot (i+1) (j+1) k     (Vec (u-1) (v-1) w) +
    knot (i+1) j (k+1)     (Vec (u-1) v (w-1)) +
    knot i (j+1) (k+1)     (Vec u (v-1) (w-1)) +
    knot (i+1) (j+1) (k+1) (Vec (u-1) (v-1) (w-1))

perlin :: Vec -> Flt
perlin v =
 let p = ((noise v)+1)*0.5
 in if p > 1 
    then error $ "perlin noise error, 1 < " ++ (show p)
    else if p < 0 
         then error $ "perlin noise error, 0 > " ++ (show p)
         else p

--untested
perlin_turb :: Vec -> Int -> Flt
perlin_turb v l =
 let p = turbulence v l
 in if p > 1 
    then error $ "perlin turbulence error, 1 < " ++ (show p)
    else if p < 0 
         then error $ "perlin turbulence error, 0 > " ++ (show p)
         else p
