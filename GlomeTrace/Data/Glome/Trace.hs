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
type TraceResult t m = (ColorA, [t], Rayint t m)
data PacketTraceResult t m =
  PacketTraceResult (TraceResult t m) (TraceResult t m)
                    (TraceResult t m) (TraceResult t m)

traceMiss = (ca_transparent, [], RayMiss)

packetTraceMiss = PacketTraceResult traceMiss traceMiss traceMiss traceMiss

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
trace :: ctxa -> Shader t m ctxa ctxb -> SolidItem t m -> Ray -> Flt -> Int -> TraceResult t m
trace _ _ _ _ _ 0 = traceMiss
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

-- | A trace function which returns some additional debugging
-- info, mainly for performance tuning.
trace_debug :: ctxa -> Shader t m ctxa ctxb -> SolidItem t m -> Ray -> Flt -> Int -> (TraceResult t m, Int)
trace_debug _ _ _ _ _ 0 = (traceMiss, 0)
trace_debug ctxa (Shader pre post miss) sld ray depth recurs =
  let (ri, dbg)  = rayint_debug sld ray depth [] []
      ctxb = pre ctxa ray sld ri
  in
    case ri of
      RayMiss -> let (c, ts) = miss ctxa ray sld in ((c, ts, ri), dbg)
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
          ((c, ts++tags, ri), dbg)

-- | Trace a packet of four rays at a time.  Sometimes, this
-- may be a performance advantage.  However, ever since my 
-- transition to typeclasses, this has not performed any better
-- than the mono-ray path.

trace_packet :: ctxa -> Shader t m ctxa ctxb -> SolidItem t m -> Ray -> Ray -> Ray -> Ray -> Flt -> Int -> PacketTraceResult t m
trace_packet _ _ _ _ _ _ _ _ 0 = packetTraceMiss
trace_packet ctxa (Shader pre post miss) sld ray1 ray2 ray3 ray4 depth recurs =
  let ri = packetint sld ray1 ray2 ray3 ray4 depth [] []
  in
    prMap func ri ray1 ray2 ray3 ray4
  
  where
    func ray ri =
      let ctxb = pre ctxa ray sld ri
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

    prMap f (PacketResult ri1 ri2 ri3 ri4) ray1 ray2 ray3 ray4 =
      PacketTraceResult (f ray1 ri1) (f ray2 ri2) (f ray3 ri3) (f ray4 ri4)
      

