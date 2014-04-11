
module Data.Glome.Shader where

import Data.Maybe(mapMaybe)
import Data.List(foldl')

import Data.Glome.Vec
import Data.Glome.Clr
import Data.Glome.Solid
import Data.Glome.Trace

--LIGHTS--
data Light = Light {
  litpos     :: !Vec,
  litcol     :: !Color,
  litfalloff :: Flt -> Flt,
  litrad     :: !Flt,
  litshadow  :: !Bool
}

-- | Construct a light given a center location and a color.
light :: Vec -> Color -> Light
light pos clr = Light pos clr (\x -> 1/(x*x)) infinity True


--MATERIALS--

-- | Surface properties at a point on an object's surface.
-- Much of this is standard whitted-style illumination.
-- Plain diffuse/specular suraces can be defined with
-- Surface.
--
-- Reflection and Refraction have their own constructors.
-- AdditiveLayers is a way of stacking textures such that
-- the colors are added together.
--
-- Blend takes two textures and returns the result of
-- cobining them.
--
-- Warp is a little stranger; it takes a ray and re-casts
-- into a separate scene (or the same one, if you so choose).

data Material t =
  Surface Color Flt Flt Flt Flt Flt Bool | -- color, alpha, ambient, diffuse, specular, shine, dielectric
  Reflect Flt | -- amount
  Refract Flt Flt Flt | -- reflection, refraction, ior
  Warp (SolidItem t (Material t))
       (SolidItem t (Material t))
       [Light]
       (Ray -> Rayint t (Material t) -> Ray) | -- frame, scene, ctx, xfm
  AdditiveLayers [Material t] |
  Blend (Material t) (Material t) Flt

-- | Uniform texture
t_uniform :: Material t -> Texture t (Material t)
t_uniform m = \_ _ -> m


--SHADER--

-- | Calculate light intensity and direction at the current ray
-- intersection.
-- We do this up front so we don't have to re-do the shadow tests when we
-- evaluate multiple layered textures.
mpreshade :: [Light] -> Ray -> SolidItem t (Material t) -> Rayint t (Material t) -> [(Color, Vec)]
mpreshade _ _ _ RayMiss = []
mpreshade lights (Ray o dir) scene (RayHit _ hitpos norm _ _ _ _) =
  mapMaybe illuminate lights
  where
    illuminate (Light lpos color falloff rad do_shadow) =
      let lvec = vsub lpos hitpos
      in if vdot lvec norm < 0
         then Nothing
         else
           let llen = vlen lvec
               ldir = vscale lvec (1/llen)
           in
             if llen > rad || (do_shadow && shadow scene (Ray (vscaleadd hitpos norm delta) ldir) (llen - (2*delta)))
             then Nothing
             else Just (cscale color (falloff llen), ldir)

mpostshade :: [Light] -> [(Color, Vec)] -> Material t -> Ray -> SolidItem t (Material t) -> Rayint t (Material t) -> Int -> (ColorA, [t])
mpostshade ls lights mat ray@(Ray o dir) s rayint recurs =
  case rayint of
    RayMiss -> (ca_transparent, [])
    RayHit d p n xfmray uvw texs _ ->
      let eyedir = vinvert dir
      in
        case mat of
          Surface color alpha amb kd ks shine dielectric ->
            let ambient = cscale color amb
                direct = foldl' cadd c_black $ map illuminate lights
                illuminate (lcolor, ldir) =
                   let halfangle = bisect ldir eyedir
                       ldotn  = fmax 0 $ vdot ldir n
                       blinn = if ks <= delta
                               then 0
                               else let b = fmax 0 $ ((vdot halfangle n) ** shine) * ldotn
                                    in if isNaN b then 0 else b
                       diffuse = vdot ldir n
                   in cscale lcolor ((blinn*ks) + (diffuse*kd))
                (Color r g b) = cadd ambient direct
                resultcolora = ColorA r g b alpha
            in
              (resultcolora, []) 

          Reflect refl -> 
            if (refl > 0) && (recurs > 0)
            then let outdir = reflect dir n 
                     (ColorA r g b a, refltags, _) =
                       (trace ls
                              materialShader
                              s
                              (Ray (vscaleadd p outdir delta) outdir) 
                              infinity 
                              (recurs-1))
                 in (ColorA r g b (a*refl), refltags)
            else (ca_black, [])

          Refract refl refr ior ->
            if (refl > 0 || refr > 0) && (recurs > 0)
            then let outdir = reflect dir n
                     (ColorA reflr reflg reflb refla, refltags, _) =
                       (trace ls
                              materialShader
                              s
                              (Ray (vscaleadd p outdir delta) outdir) 
                              infinity
                              (recurs-1))

                     -- Formula from: http://steve.hollasch.net/cgindex/render/refraction.txt
                     -- It seems to be sort of working, but rays seem unable to escape...
                     eta = if (vdot n eyedir) > 0 then ior else 1/ior

                     c1 = (vdot dir n)
                     cs2 = 1 - (eta * eta) * (1 - (c1 * c1))
                     (ColorA refrr refrg refrb refra, refrtags, _) =
                       if cs2 < 0
                       then (ca_black, [], raymiss)
                       else 
                         let t = vadd (vscale dir eta) (vscale n (eta * c1 - (sqrt cs2)))
                         in (trace ls
                                   materialShader
                                   s
                                   (Ray (vscaleadd p t delta) t)
                                   infinity
                                   (recurs-1))


                 in (ColorA (reflr * refl + refrr * refr)
                            (reflg * refl + refrg * refr)
                            (reflb * refl + refrb * refr)
                            (refla * refl + refra * refr), (refltags ++ refrtags))

            else (ca_transparent, [])

          Warp frame scene' lights' xfm ->
            let (fcolor, ftags, fint) =
                  (trace ls
                         materialShader
                         frame
                         xfmray 
                         infinity
                         (recurs-1))
                (wcolor, wtags, wint) =
                  (trace lights'
                         materialShader
                         scene'
                         (xfm ray rayint)
                         (ridepth fint)
                         (recurs-1))
            in
               if ridepth fint < ridepth wint
               then (fcolor, ftags)
               else (wcolor, wtags)

          AdditiveLayers ms ->
            let (cs, taglists) = unzip $ map (\m -> mpostshade ls lights m ray s rayint recurs) ms
            in (casum cs, (concat taglists))

          Blend ma mb weight ->
            let (ca, tagsa) = mpostshade ls lights ma ray s rayint recurs
                (cb, tagsb) = mpostshade ls lights mb ray s rayint recurs
            in (caweight ca cb weight, tagsa ++ tagsb)

mmissshade :: [Light] -> Ray -> SolidItem t (Material t) -> (ColorA, [t])
mmissshade _ _ _ = (ca_transparent, [])

materialShader = Shader mpreshade mpostshade mmissshade

{-
-- no shadows, reflection, or lighting
flat_shade :: Rayint t -> Ray -> Scene t -> Int -> Int -> ColorA
flat_shade ri (Ray o indir) scn recurs debug =
 case ri of
  RayMiss -> bground scn
  RayHit d p n t -> 
   let (Material clr refl refr ior kd ks shine) = t ri
   in liftcolor clr
-}
