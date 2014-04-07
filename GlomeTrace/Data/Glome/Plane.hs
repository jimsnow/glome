{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Glome.Plane (plane, plane_offset) where
import Data.Glome.Vec
import Data.Glome.Solid

-- A plane is effectively a half-space; everything below the plane is
-- "inside", everything above is "outside".

data Plane t m = Plane Vec Flt deriving Show -- normal, perpendicular offset from origin

-- | Construct a plane (or, more accurately, a half-space)
-- by specifying a point on the plane and a normal.
-- The normal points towards the outside of the plane.
-- Planes are often useful within CSG objects.
plane :: Vec -> Vec -> SolidItem t m
plane orig norm_ = SolidItem $ Plane norm d
 where norm = vnorm norm_
       d = vdot orig norm

-- we can also specify a point and a perpindicular offset:

plane_offset :: Vec -> Flt -> SolidItem t m
plane_offset pt off = SolidItem $ Plane pt off

rayint_plane :: Plane tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_plane (Plane norm offset) ray@(Ray orig dir) d t tags =
 let hit = -(((vdot norm orig)-offset) / (vdot norm dir))
 in if hit < 0 || hit > d 
    then RayMiss
    else RayHit hit (vscaleadd orig dir hit) norm ray vzero t tags

inside_plane :: Plane tag mat -> Vec -> Bool
inside_plane (Plane norm offset) pt =
 let onplane = (vscale norm offset)
     newvec = vsub onplane pt
 in vdot newvec norm > 0

-- Note: attempting to use an infinite object (such as
-- a plane) inside a bih will cause an exception.

bound_plane :: Plane tag mat -> Bbox
bound_plane (Plane norm offset) = everything_bbox

instance Solid (Plane t m) t m where
 rayint = rayint_plane
 inside = inside_plane
 bound = bound_plane
