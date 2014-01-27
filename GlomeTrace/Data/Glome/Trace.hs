{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

module Data.Glome.Trace where
import Data.Glome.Scene
import Data.List

{-
We put lighting code in this file because it needs to be 
mutually recursive with the trace function, for refraction
and reflection.
 -}

-- | Result of tracing a packet of 4 rays at once.
data PacketColor = PacketColor !Color !Color !Color !Color

{-
class (Show a) => Shader a where
 -- ray intersection, scene, recursion limit
 shade :: Rayint -> Ray -> Scene -> Int -> Color
 shadepacket :: PacketResult -> Ray -> Ray -> Ray -> Ray -> Scene -> Int -> PacketColor

 shadepacket (PacketResult ri1 ri2 ri3 ri4) r1 r2 r3 r4 scene recurs =
  PacketColor (shade ri1 r1 scene recurs)
              (shade ri2 r2 scene recurs)
              (shade ri3 r3 scene recurs)
              (shade ri4 r4 scene recurs)
-}

{-
simple_shade :: Rayint -> [Light] -> Solid -> Color -> Color
simple_shade ri lights s bg =
 case ri of
  (RayHit d p n t) ->
   let (Material clr refl refr ior kd shine) = t ri
   in cscale clr (vdot n (Vec 0.0 1.0 0.0))
  (RayMiss) -> bg
-}

-- set rgb to normal's xyz coordinates
-- as a debugging aid
debug_norm_shade :: Rayint -> Ray -> Scene -> Int -> Int -> Color
debug_norm_shade ri (Ray o indir) scn recurs debug =
 case ri of
  RayHit d p (Vec nx ny nz) t -> (Color (fabs $ nx/2) (fabs $ ny/2) (fabs $ nz/2))
  RayMiss -> bground scn

-- no shadows, reflection, or lighting
flat_shade :: Rayint -> Ray -> Scene -> Int -> Int -> Color
flat_shade ri (Ray o indir) scn recurs debug =
 case ri of
  RayMiss -> bground scn
  RayHit d p n t -> 
   let (Material clr refl refr ior kd ks shine) = t ri
   in clr

-- | This is the lighting routine that handles diffuse light, shadows, 
-- specular highlights and reflection.  Given a ray intersection, the ray,
-- a scene, and a recursion limit, return a color.  "Debug" is a parameter
-- useful for debugging; sometimes we might want to tint the color by 
-- the number of bounding boxes tested or something similar.
-- Todo: refraction
shade :: Rayint  -- ^ ray intersection returned by rayint
      -> Ray     -- ^ ray that resuted in the ray intersection
      -> Scene   -- ^ scene we're rendering
      -> Int     -- ^ recursion limit
      -> Int     -- ^ debugging value (usualy not used)
      -> Color   -- ^ computed color
shade ri (Ray o indir) scn recurs !debug = 
 case ri of
  (RayHit d p n t) ->
   let (Material clr refl_ refr ior kd ks shine) = t ri
       s    = sld scn
       lights = lits scn
       direct = foldl' cadd c_black 
                 (map (\ (Light lp lc lexp lrad lshadow) ->
                   let eyedir = vinvert indir
                       lvec = vsub lp p
                       llen = vlen lvec
                       ldir = vscale lvec (1.0/llen)   
                       halfangle = bisect ldir eyedir
                       ldotn  = fmax 0 $ vdot ldir n
                       blinn = if ks <= delta
                               then 0
                               else let b = fmax 0 $ ((vdot halfangle n) ** shine) * ldotn
                                    in if isNaN b then 0 else b
                       -- indotn = fmax 0 $ vdot eyedir n
                       intensity = llen ** lexp
                       --intensity = 0.2
                   in
                    if vdot n lvec < 0 
                    then c_black
                    else
                     if llen > lrad || (lshadow && shadow s (Ray (vscaleadd p n delta) ldir) (llen-(2*delta)))
                     then c_black
                     else
                       (cadd 
                         -- diffuse
                         (cmul clr (cscale lc (ldotn * intensity * kd)))
                         -- blinn/torrance-sparrow  highlight (pbrt p 440)
                         (cscale lc (blinn * intensity * ks)) ))
                  lights)
       reflect_ = 
         if (refl_ > delta) && (recurs > 0)
         then let outdir = reflect indir n 
              in cscale (trace scn 
                               (Ray (vscaleadd p outdir delta) outdir) 
                               infinity (recurs-1) ) refl_
         else c_black
       refract = 
         if (refr > delta) && (recurs > 0)
         then c_black
         else c_black
       in
         cadd direct $ cadd reflect_ refract

  (RayMiss) -> bground scn

-- | Given a scene, a ray, a maximum distance, and a maximum
-- recursion depth, test the ray for intersection against 
-- the object within the scene, then pass the ray intersection
-- to the shade routine (which may trace secondary rays of its 
-- own), which returns a color.  For most applications, this is
-- the entry point into the ray tracer.
trace :: Scene -> Ray -> Flt -> Int -> Color
trace scn ray depth recurs =
 let (Scene sld lights cam dtex bgcolor) = scn 
 in shade (rayint sld ray depth dtex) ray scn recurs 0
         
-- | Similar to trace, but return depth as well as color.
-- We might want the depth for post-processing effects.
trace_depth :: Scene -> Ray -> Flt -> Int -> (Color,Flt)
trace_depth scn ray depth recurs =
 let (Scene sld lights cam dtex bgcolor) = scn 
     ri = rayint sld ray depth dtex 
     d = case ri of
          RayHit d_ _ _ _ -> d_
          RayMiss -> infinity
     clr = shade ri ray scn recurs 0
 in (clr,d)

-- | Similar to trace, but return hit position as well as color.
trace_pos :: Scene -> Ray -> Flt -> Int -> (Color,Vec)
trace_pos scn ray depth recurs =
 let (Scene sld lights cam dtex bgcolor) = scn 
     ri = rayint sld ray depth dtex 
     p = case ri of
          RayHit _ p _ _ -> p
          RayMiss -> (Vec 0 0 0) -- fixme
     clr = shade ri ray scn recurs 0
 in (clr,p)

-- | A trace function which returns some additional debugging
-- info, mainly for performance tuning.
trace_debug :: Scene -> Ray -> Flt -> Int -> Color
trace_debug scn ray depth recurs =
 let (Scene sld lights cam dtex bgcolor) = scn
     (ri,n) = rayint_debug sld ray depth dtex
 in 
  cadd (shade ri ray scn recurs 0) (Color 0 ((fromIntegral (Prelude.abs n)) * 0.01) 0)

-- | Trace a packet of four rays at a time.  Sometimes, this
-- may be a performance advantage.  However, ever since my 
-- transition to typeclasses, this has not performed any better
-- than the mono-ray path.
trace_packet :: Scene -> Ray -> Ray -> Ray -> Ray -> Flt -> Int -> PacketColor
trace_packet scn ray1 ray2 ray3 ray4 depth recurs =
 let (Scene sld lights cam dtex bgcolor) = scn
     PacketResult ri1 ri2 ri3 ri4 = packetint sld ray1 ray2 ray3 ray4 depth dtex
 in PacketColor (shade ri1 ray1 scn recurs 0)
                (shade ri2 ray2 scn recurs 0)
                (shade ri3 ray3 scn recurs 0)
                (shade ri4 ray4 scn recurs 0)
