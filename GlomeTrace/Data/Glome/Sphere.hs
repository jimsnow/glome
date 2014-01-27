{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

module Data.Glome.Sphere (sphere) where
import Data.Glome.Vec
import Data.Glome.Solid

-- | center, radius, 1/radius
data Sphere = Sphere !Vec !Flt !Flt deriving Show


-- | Construct a sphere given a center location and a radius.
sphere :: Vec -> Flt -> SolidItem
sphere c r =
 SolidItem (Sphere c r (1.0/r))

-- adapted from graphics gems volume 1
rayint_sphere :: Sphere -> Ray -> Flt -> Texture -> Rayint
rayint_sphere (Sphere center r invr) (Ray e dir) dist t = 
 let eo = vsub center e
     v  = vdot eo dir
     vsqr = v*v
     csqr = vdot eo eo
     rsqr = r*r
     disc = rsqr - (csqr - vsqr)
 in
  if disc < 0.0 then
   RayMiss
  else
   let d = sqrt disc
       hitdist = if (v-d) > 0 then (v-d) else (v+d)
   in if (hitdist < 0) || (hitdist > dist)
      then RayMiss
      else
       let p = vscaleadd e dir hitdist
           -- n = vscale (vsub p center) invr in
           -- n = vsub (vscale p invr) (vscale center invr) in
           n = vnorm (vsub p center) 
       in RayHit hitdist p n t


packetint_sphere :: Sphere -> Ray -> Ray -> Ray -> Ray -> Flt -> Texture -> PacketResult
packetint_sphere s !r1 !r2 !r3 !r4 !d t =
 PacketResult (rayint_sphere s r1 d t)
              (rayint_sphere s r2 d t)
              (rayint_sphere s r3 d t)
              (rayint_sphere s r4 d t)

shadow_sphere :: Sphere -> Ray -> Flt -> Bool
shadow_sphere (Sphere center r invr) (Ray e dir) dist = 
 let eo = vsub center e
     v  = vdot eo dir
 in
 if (dist >= (v - r)) && (v > 0.0)
 then
  let vsqr = v*v
      csqr = vdot eo eo
      rsqr = r*r
      disc = rsqr - (csqr - vsqr) in
  if disc < 0.0 then
  False
  else
   let d = sqrt disc
       hitdist = if (v-d) > 0 then (v-d) else (v+d)
   in if (hitdist < 0) || (hitdist > dist)
      then False
      else True
 else
  False

inside_sphere :: Sphere -> Vec -> Bool
inside_sphere (Sphere center r invr) pt =
 let offset = vsub center pt 
 in (vdot offset offset) < r*r

bound_sphere :: Sphere -> Bbox
bound_sphere (Sphere center r invr) =
 let offset = (vec r r r) in
 (Bbox (vsub center offset) (vadd center offset))

instance Solid Sphere where 
 rayint = rayint_sphere
 packetint = packetint_sphere
 shadow = shadow_sphere
 inside = inside_sphere
 bound  = bound_sphere
