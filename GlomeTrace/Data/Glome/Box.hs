{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Glome.Box (box) where
import Data.Glome.Vec
import Data.Glome.Solid

-- Simple, axis-aligned bounding box defined with two points at opposing corners.

data Box t m = Box !Bbox deriving Show

box :: Vec -> Vec -> SolidItem t m
box (Vec x1 y1 z1) (Vec x2 y2 z2) =
 SolidItem (Box (Bbox (Vec (fmin x1 x2) (fmin y1 y2) (fmin z1 z2))
                      (Vec (fmax x1 x2) (fmax y1 y2) (fmax z1 z2))))

-- this could be optimized a bit more
rayint_box :: Box tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_box (Box (Bbox (Vec p1x p1y p1z) (Vec p2x p2y p2z))) r@(Ray orig@(Vec ox oy oz) dir@(Vec dx dy dz)) d t tags =
 let dxrcp = 1/dx
     dyrcp = 1/dy
     dzrcp = 1/dz
     Interval inx outx = if dx > 0 
                         then Interval ((p1x-ox)*dxrcp) ((p2x-ox)*dxrcp)
                         else Interval ((p2x-ox)*dxrcp) ((p1x-ox)*dxrcp)
     Interval iny outy = if dy > 0
                         then Interval ((p1y-oy)*dyrcp) ((p2y-oy)*dyrcp)
                         else Interval ((p2y-oy)*dyrcp) ((p1y-oy)*dyrcp)
     Interval inz outz = if dz > 0
                         then Interval ((p1z-oz)*dzrcp) ((p2z-oz)*dzrcp)
                         else Interval ((p2z-oz)*dzrcp) ((p1z-oz)*dzrcp)
     lastin   = (fmax3 inx iny inz)
     firstout = (fmin3 outx outy outz)
 in if lastin > firstout || firstout < 0 || lastin > d
    then RayMiss
    else
      if lastin < 0
      then -- origin is inside
        let n = if outx == firstout
                then if dx > 0 then vx else nvx
                else if outy == firstout
                     then if dy > 0 then vy else nvy
                     else if dz > 0 then vz else nvz
        in
            RayHit firstout (vscaleadd orig dir firstout) n r vzero t tags

      else -- origin is outside
        let n = if inx == lastin 
                then if dx > 0 then nvx else vx
                else if iny == lastin
                     then if dy > 0 then nvy else vy
                     else if dz > 0 then nvz else vz
        in
            RayHit lastin (vscaleadd orig dir lastin) n r vzero t tags

shadow_box :: Box t m -> Ray -> Flt -> Bool
shadow_box (Box box) r d =
 let Interval near far = bbclip r box 
 in
  if (near > far) || far <= 0 || far > d
  then False
  else True

inside_box :: Box t m -> Vec -> Bool
inside_box (Box (Bbox (Vec x1 y1 z1) (Vec x2 y2 z2))) (Vec x y z) =
 x > x1 && x < x2 && 
 y > y1 && y < y2 && 
 z > z1 && z < z2

bound_box :: Box t m -> Bbox
bound_box (Box box) = box

instance Solid (Box t m) t m where
 rayint = rayint_box
 shadow = shadow_box
 inside = inside_box
 bound = bound_box
