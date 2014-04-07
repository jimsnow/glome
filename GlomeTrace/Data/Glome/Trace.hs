{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

module Data.Glome.Trace where
import Data.Glome.Scene
import Data.List

-- The complex type here allows us to do some precomputation of values that we
-- might need in layered textures that we don't want to computer over again.
-- For instance, if we have multiple textures we probably don't want to
-- re-compute the lighting.
-- We need a separate shader for a miss because in that case there are no
-- materials.

data Shader t m ctxa ctxb = Shader {
  preshade  :: ctxa -> Ray -> SolidItem t m -> Rayint t m -> ctxb,
  postshade :: ctxa -> ctxb -> m -> Ray -> SolidItem t m -> Rayint t m -> Int -> (ColorA, [t]),
  missshade :: ctxa -> Ray -> SolidItem t m -> (ColorA, [t])
}

-- | Result of tracing a packet of 4 rays at once.
data PacketColor = PacketColor !Color !Color !Color !Color

{-
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
-}

opaque :: ColorA -> Bool
opaque (ColorA _ _ _ a) = a+delta >= 1

-- | Given a scene, a shader, a ray, a maximum distance, and a maximum
-- recursion depth, test the ray for intersection against 
-- the object within the scene, then pass the ray intersection
-- to the shade routine (which may trace secondary rays of its 
-- own), which returns a color.  For most applications, this is
-- the entry point into the ray tracer.
trace :: ctxa -> Shader t m ctxa ctxb -> SolidItem t m -> Ray -> Flt -> Int -> (ColorA, [t], Rayint t m)
trace _ _ _ _ _ 0 = (ca_transparent, [], RayMiss)
trace ctxa (Shader pre post miss) sld ray depth recurs =
  let ri  = rayint sld ray depth [] []
      ctxb = pre ctxa ray sld ri
  in
    case ri of
      RayMiss -> let (c, ts) = miss ctxa ray sld in (c, ts, ri)
      RayHit _ _ _ _ _ texs tags ->
        let
          (c, ts) =
            foldl
              (\acc@(colora, tagsa) tex ->
                if opaque colora
                then acc
                else
                  let (colorb, tagsb) = post ctxa ctxb (tex ray ri) ray sld ri recurs
                  in
                     (cafold colora colorb, tagsb ++ tagsa)
              )
              (ca_transparent, [])
              texs
        in
          (c, ts++tags, ri)

{-
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

-}
