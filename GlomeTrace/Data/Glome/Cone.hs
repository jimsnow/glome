{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

module Data.Glome.Cone (disc, cone, cylinder) where
import Data.Glome.Vec
import Data.Glome.Solid
import Data.Glome.Sphere -- for disc bounding box

-- We define "Cone", "Cylinder", and "Disc" in this module.
-- A Cone is really a tapered cylinder with a different radius
-- at each end, though the underlying representation is a
-- clipped cone.

-- We represent Cylinders and Cones as transformations of axis-aligned
-- primitives.

-- Todo: cylinder shadow test

data Disc = Disc !Vec !Vec !Flt deriving Show -- position, normal, r*r
data Cylinder = Cylinder !Flt !Flt !Flt deriving Show -- radius height1 height2
data Cone = Cone !Flt !Flt !Flt !Flt deriving Show -- r clip1 clip2 height

-- CONSTRUCTORS --

-- | Create a disc.  These are used as the end-caps on cones and cylinders,
-- but they can be constructed by themselves as well.
disc :: Vec -> Vec -> Flt -> SolidItem
disc pos norm r =
 SolidItem $ Disc pos norm (r*r)

cylinder_z :: Flt -> Flt -> Flt -> SolidItem
cylinder_z r h1 h2 = SolidItem (Cylinder r h1 h2)

cone_z :: Flt -> Flt -> Flt -> Flt -> SolidItem
cone_z r h1 h2 height = SolidItem (Cone r h1 h2 height)

-- | Construct a general cylinder from p1 to p2 with radius r.
cylinder :: Vec -> Vec -> Flt -> SolidItem
cylinder p1 p2 r =
 let axis = vsub p2 p1
     len  = vlen axis
     ax1  = vscale axis (1/len)
     (ax2,ax3) = orth ax1 
 in transform (cylinder_z r 0 len)
              [ (xyz_to_uvw ax2 ax3 ax1),
                (translate p1) ]
                        
-- | Construct a cone from p1 to p2.  R1 and r2 are the radii at each
-- end.  A cone need not come to a point at either end.
cone :: Vec -> Flt -> Vec -> Flt -> SolidItem
cone p1 r1 p2 r2 =
 if r1 < r2 
 then cone p2 r2 p1 r1
 else if r1-r2 < delta
      then cylinder p1 p2 r2
      else
        let axis = vsub p2 p1
            len  = vlen axis
            ax1  = vscale axis (1/len)
            (ax2,ax3) = orth ax1 
            height = (r1*len)/(r1-r2) -- distance to end point
        in
         transform (cone_z r1 0 len height)
                   [ (xyz_to_uvw ax2 ax3 ax1),
                     (translate p1) ]                 

rayint_disc :: Disc -> Ray -> Flt -> Texture -> Rayint
rayint_disc (Disc point norm radius_sqr) r@(Ray orig dir) d t =
 let dist = plane_int_dist r point norm 
 in if dist < 0 || dist > d 
    then RayMiss
    else let pos = vscaleadd orig dir dist
             offset = vsub pos point
         in 
          if (vdot offset offset) > radius_sqr
          then RayMiss
          else RayHit dist pos norm t

shadow_disc :: Disc -> Ray -> Flt -> Bool
shadow_disc (Disc point norm radius_sqr) !r@(Ray orig dir) !d =
 let dist = plane_int_dist r point norm 
 in if dist < 0 || dist > d 
    then False
    else let pos = vscaleadd orig dir dist
             offset = vsub pos point
         in 
          if (vdot offset offset) > radius_sqr
          then False
          else True

bound_disc :: Disc -> Bbox
bound_disc (Disc pos norm rsqr) =
 bound (sphere pos (sqrt rsqr))

instance Solid Disc where
 rayint = rayint_disc
 shadow = shadow_disc
 inside (Disc _ _ _) _ = False
 bound = bound_disc


rayint_cylinder :: Cylinder -> Ray -> Flt -> Texture -> Rayint
rayint_cylinder (Cylinder r h1 h2) ray@(Ray orig@(Vec ox oy oz) dir@(Vec dx dy dz)) d t =
 let a = dx*dx + dy*dy
     b = 2*(dx*ox + dy*oy)
     c = ox*ox + oy*oy - r*r
     disc = b*b - 4*a*c
 in  if disc < 0 
     then RayMiss
     else 
      let discsqrt = sqrt disc 
          q = if b < 0 
              then (b-discsqrt)*(-0.5)
              else (b+discsqrt)*(-0.5)
          t0' = q/a
          t1' = c/q
          t0  = fmin t0' t1'
          t1  = fmax t0' t1'
      in if t1 < 0 || t0 > d 
         then RayMiss
         else let dist = if t0 < 0
                         then t1
                         else t0
              in if dist < 0 || dist > d
                 then RayMiss
                 else let pos@(Vec posx posy posz) = vscaleadd orig dir dist
                      in if posz > h1 && posz < h2
                         then RayHit dist pos (Vec (posx/r) (posy/r) 0) t
                         else if dz > 0 -- ray pointing up from bottom
                              then if oz < h1
                                   then rayint_disc (Disc (Vec 0 0 h1) nvz (r*r)) ray d t
                                   --then rayint_aadisc h1 r ray d t
                                   else RayMiss
                              else if oz > h2
                                   then rayint_disc (Disc (Vec 0 0 h2) vz (r*r)) ray d t
                                   --rayint_aadisc h2 r ray d t -- todo: fix normal
                                   else RayMiss

inside_cylinder :: Cylinder -> Vec -> Bool
inside_cylinder (Cylinder r h1 h2) (Vec x y z) =
 z > h1 && z < h2 && x*x + y*y < r*r
  
bound_cylinder :: Cylinder -> Bbox
bound_cylinder (Cylinder r h1 h2) =
 Bbox (Vec (-r) (-r) h1) (Vec r r h2)

instance Solid Cylinder where
 rayint = rayint_cylinder
 inside = inside_cylinder
 bound = bound_cylinder


rayint_cone :: Cone -> Ray -> Flt -> Texture -> Rayint
rayint_cone (Cone r clip1 clip2 height) ray@(Ray orig@(Vec ox oy oz) dir@(Vec dx dy dz)) d t =
 let k' = (r/height)
     k = k'*k'
     a = dx*dx + dy*dy - k*dz*dz
     b = 2*(dx*ox + dy*oy - k*dz*(oz-height))
     c = ox*ox + oy*oy - k*(oz-height)*(oz-height)
     disc = b*b - 4*a*c
 in if disc < 0
    then RayMiss
    else
     let discsqrt = sqrt disc
         q = if b < 0
             then (b-discsqrt)*(-0.5)
             else (b+discsqrt)*(-0.5)
         t0' = q/a
         t1' = c/q
         t0  = fmin t0' t1'
         t1  = fmax t0' t1'
     in if t1 < 0 || t0 > d 
        then RayMiss
        else let dist = if t0 < 0
                        then t1
                        else t0
             in if dist < 0 || dist > d
                then RayMiss
                else
                 let pos = vscaleadd orig dir dist
                     Vec posx posy posz = pos
                 in if posz > clip1 && posz < clip2
                    then let invhyp = 1 / (sqrt (height*height + r*r))
                             up  = r * invhyp
                             out = height * invhyp
                             r_ = sqrt (posx*posx + posy*posy)
                             correction = (out)/(r_)
                         in RayHit dist pos (Vec (posx*correction) (posy*correction) up) t
                    else 
                     if dz > 0 -- ray pointing up from bottom
                     then if oz < clip1
                          then rayint_disc (Disc (Vec 0 0 clip1) nvz (r*r)) ray d t
                          else RayMiss
                     else if oz > clip2
                          then let r2 = r*(1-((clip2-clip1)/(height)))
                               in rayint_disc (Disc (Vec 0 0 clip2) vz (r2*r2)) ray d t
                                   --rayint_aadisc clip2 r2 ray d t
                          else RayMiss
                             -- then rayint_aadisc clip1 r ray d t
                             -- else RayMiss -- rayint_aadisc clip2 
                                              --   (r*((clip2-clip1)/height)) 
                                               --  (Ray orig dir) d t -- todo: fix normal

shadow_cone :: Cone -> Ray -> Flt -> Bool
shadow_cone (Cone r clip1 clip2 height) ray@(Ray orig@(Vec ox oy oz) dir@(Vec dx dy dz)) d =
 let k' = (r/height)
     k = k'*k'
     a = dx*dx + dy*dy - k*dz*dz
     b = 2*(dx*ox + dy*oy - k*dz*(oz-height))
     c = ox*ox + oy*oy - k*(oz-height)*(oz-height)
     disc = b*b - 4*a*c
 in if disc < 0
    then False
    else
     let discsqrt = sqrt disc
         q = if b < 0
             then (b-discsqrt)*(-0.5)
             else (b+discsqrt)*(-0.5)
         t0' = q/a
         t1' = c/q
         t0  = fmin t0' t1'
         t1  = fmax t0' t1'
     in if t1 < 0 || t0 > d 
        then False
        else let dist = if t0 < 0
                        then t1
                        else t0
             in if dist < 0 || dist > d
                then False
                else
                 let pos = vscaleadd orig dir dist
                     Vec posx posy posz = pos
                 in if posz > clip1 && posz < clip2
                    then True
                    else 
                     if dz > 0 -- ray pointing up from bottom
                     then if oz < clip1
                          then shadow (Disc (Vec 0 0 clip1) nvz (r*r)) ray d
                          else False
                     else if oz > clip2
                          then let r2 = r*(1-((clip2-clip1)/(height)))
                               in shadow (Disc (Vec 0 0 clip2) vz (r2*r2)) ray d
                          else False


inside_cone :: Cone -> Vec -> Bool
inside_cone (Cone rbase h1 h2 height) (Vec x y z) =
 let r = rbase*(1-(((z-h1)/height)))
 in z > h1 && z < h2 && x*x + y*y < r*r

bound_cone :: Cone -> Bbox
bound_cone (Cone r h1 h2 height) =
 Bbox (Vec (-r) (-r) h1) (Vec r r h2)

instance Solid Cone where
 rayint = rayint_cone
 shadow = shadow_cone
 inside = inside_cone
 bound  = bound_cone
