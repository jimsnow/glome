{-# LANGUAGE ScopedTypeVariables #-}

module TestScene (scn, Scene, Tag, T, M, SI) where
import Data.Glome.Scene
import Data.List hiding (group)
import Data.Glome.Texture
import Data.Glome.Shader
import Data.Glome.Trace
import System.Random

type Tag   = String
type T     = Texture Tag M
type M     = Material Tag
type SI    = SolidItem Tag M
type Scene = (SI, [Light], Camera, Shader Tag M [Light] [(Color, Vec)])

lights = [ light (Vec (-100) 70 (140)) (cscale (Color 1 0.8 0.8) 7000)
         , light (Vec (-3) 5 8) (cscale (Color 1.5 2 2) 10)
         ] 

lattice = 
 let n = 10 :: Flt
 in bih [sphere (vec x y z) 0.2 | x <- [(-n)..n],
                                  y <- [(-n)..n],
                                  z <- [(-n)..n]]

icosahedron pos r = 
 let gr = ((1+(sqrt 5))/2) -- golden ratio, 1.618033988749895
     n11 = [(-r),r]
     ngrgr = [(-gr)*r,gr*r]
     grrcp = [((-r)/gr),(r/gr)]
     points = [Vec x y z | x <- n11,
                           y <- n11,
                           z <- n11 ] ++
              [Vec 0 y z | y <- grrcp,
                           z <- ngrgr ] ++
              [Vec x y 0 | x <- grrcp,
                           y <- ngrgr] ++
              [Vec x 0 z | x <- ngrgr,
                           z <- grrcp]
     pln x = (plane_offset (vnorm x) (r+(vdot (vnorm x) pos)))
 in
  tag (intersection ((sphere pos (1.26*r)):(map pln points))) "icosahedron"

dodecahedron pos r =
 let gr = (1+(sqrt 5))/2 -- golden ratio, 1.618033988749895
     n11 = [(-r),r]
     ngrgr = [(-gr)*r,gr*r]
     points = [Vec 0 y z | y <- n11, z <- ngrgr] ++
              [Vec x 0 z | z <- n11, x <- ngrgr] ++
              [Vec x y 0 | x <- n11, y <- ngrgr]
     pln x = (plane_offset (vnorm x) (r+(vdot (vnorm x) pos)))
 in
  tag (intersection ((sphere pos (1.26*r)):(map pln points))) "dodecahedron"

spiral = [ ((Vec ((sin (rot n))*n) 
                 ((cos (rot n))*n) 
                 (n-3)), (n/15)) | n <- [0, 0.01..6]]
                               

coil = bih (zipWith (\ (p1,r1) (p2,r2) -> (group [(cone p1 r1 p2 r2), 
                                                  (sphere p1 r1)] )) 
                    spiral 
                    (tail spiral))

-- we branch once per year
-- not really a plausible oak, but it's getting there
oak :: Flt -> StdGen -> SI
oak age rng = 
 if age < 0 
 then nothing
 else 
  let year :: Int   = floor age
      season = age-(fromIntegral year)
      thickness = 0.03
      minbranch = deg 10
      maxbranch = deg 25
      tree 0 r = nothing
      tree 1 r = -- cone (Vec 0 0 0) thickness (Vec 0 season 0) 0
                 tex (sphere (Vec 0 0 0) season) (t_matte (Color 0.2 1 0.4))
      tree n_ r = let nf = fromIntegral n_ 
                      height_ = nf
                      (rng1,rng2) = split r
                      (rng3,rng4) = split rng1
                      (r1,rng5)   = randomR (0,0.5) rng4
                      (r2,rng6)   = randomR (minbranch,maxbranch) rng5
                      (r3,rng7)   = randomR (0.8,0.95) rng6
                      (r4,rng8)   = randomR (0.0,1.0) rng7
                      seglen      = 0.5 + r1
                      branchang   = r2
                      scaling     = r3
                      (height,n)  = if r4 > (1 :: Float)
                                    then ((height_/2),(ceiling (nf/2)))
                                    else (height_, n_)
                     
                 -- we make our own manual bounding heirarchy
                 -- (bih doesn't know what to do with heirachies
                 -- of transformed objects)
                  in bound_object (sphere (Vec 0 (height/2) 0) (height/2))
                           (group [ cone (Vec 0 0 0) (thickness*height) (Vec 0 seglen 0) (thickness*(height-1)*scaling)
                                  , transform (tree (n-1) rng2) [(scale (Vec scaling scaling scaling)),
                                                                 (rotate (Vec 0 0 1) branchang),
                                                                 (rotate (Vec 0 1 0) (deg 30)),
                                                                 (translate (Vec 0 seglen 0))]
                                  , transform (tree (n-1) rng3) [(scale (Vec scaling scaling scaling)),
                                                                 (rotate (Vec 0 0 1) (-branchang)),
                                                                 (rotate (Vec 0 1 0) (deg 30)),
                                                                 (translate (Vec 0 seglen 0))]
                           ])
  in tag (tex (bih (tolist (SolidItem (flatten_transform (tree year rng))))) (t_matte (Color 0.8 0.5 0.4))) "tree"

sphereint = intersection [ (sphere (Vec (-1) 0 0) 2), 
                           (sphere (Vec 1 0 0) 2),
                           (sphere (Vec 0 (-1) 0) 2),
                           (sphere (Vec 0 1 0) 2) ]

geom = group [ tex (plane (Vec 0 0 0) (Vec 0 1 0)) (t_matte (Color 0 0.8 0.3)),
               bih [ tex (dodecahedron (Vec (-6) 3 0) 1) t_stripe
                   , tex (transform (icosahedron (Vec 4 1.5 3) 1.5) [ rotate vz (deg 11)
                                                                    , rotate vx (deg 7) ] ) t_mottled
                   , transform (oak 11.6 (mkStdGen 42)) [ scale (Vec 1.5 1.5 1.5)]
                   , tex (transform (coil) [ scale (Vec (1/3) (1/3) (1/3))
                                           , rotate (Vec 0 1 0) (deg 65)
                                           , translate (Vec (-3.5) 1 (5)) 
                                           ]) t_mirror
                   , cone (Vec (-6) 0 0) 1 (Vec (-6) 3 0) 0
                   , tex (difference (sphere (Vec 0 (-4) 5) 4.7) (sphere (Vec 1.5 (1.5) 5.2) 1.6)) t_mirror
                   , transform (tex sphereint (t_matte (Color 0.5 0 1))) [ scale (Vec 0.6 0.6 0.6),
                                                                           translate (Vec (-5.2) 1 5)]
                   ]
             ]

geom' = group [ (box (Vec (-1) (-1) (-1)) (Vec 1 1 1)),
                (group [ (sphere (Vec 2 3 0) 1), 
                         (sphere (Vec (-3) (4) 1) 0.8) ]) ]

-- cust_cam = camera (vec (-2) (5.3) (20)) (vec 0 5 0) (vec 0 1 0) 45
cust_cam = camera (vec (-2) (4.3) (15)) (vec 0 2 0) (vec 0 1 0) 45

chessboard =
  group
   [tex
      (box (vec (x-(1/2)) (-3) (z-(1/2)))
           (vec (x+(1/2)) (f x z) (z+(1/2))))
      (tf x z) | x <- [(-3.5)..3.5], z <- [(-3.5)..3.5]]
  where
   f x y = (x*y)/40
   tf x y = if mod ((floor x) + (floor y)) 2 == 0
            then t_shiny_white
            else t_mottled

portal height width thickness =
  let frame =
        tag 
          (tex
            (difference
              (box (vec (-width) 0 (-thickness))
                   (vec width height thickness))

              (box (vec (thickness-width) (thickness) (-(thickness+delta)))
                   (vec (width-thickness) (height-thickness) (thickness+delta))))
            (t_matte (Color 0.4 0.4 0.8)))
        "door frame"

      surface =
        (box (vec (-width) 0 (-delta))
             (vec (width) (height-delta) delta))

      xfm ray@(Ray origin dir) (RayHit depth pos norm _ _ _ _) =
        xfm_ray (compose [ rotate vx (deg (-85))
                         , translate (vec (8) 40 (-4))
                         ])
                (Ray pos (vnorm dir))


        --Ray (vadd pos (vec 0 15 0)) (vrotate pos (Ray pos vx) (deg 90))

  in group [frame,
            tex surface (t_uniform (Warp frame geom'' lights xfm))]
                    


geom'' =
  bih
   [ difference (transform chessboard [scale (vec 2 1.2 2)]) (tex (sphere (vec 4 1.5 3) 3.5) t_shiny_white)
   , tex (dodecahedron (vec (-6) 3 0) 1) t_stripe
   , tex (transform (icosahedron (Vec 4 1.5 3) 1.5) [ rotate vz (deg 11)
                                                    , rotate vx (deg 7) ] ) t_mottled
   , cone (Vec (-6) (-1) 0) 0.7 (Vec (-6) 3 0) 0
   , transform (oak 11.4 (mkStdGen 42)) [ scale (Vec 2 2 2), translate (vec 2 (-1) (-8))]
   , tex (difference (transform (lattice) [rotate vz (deg 23),
                                           rotate vx (deg 43),
                                           scale (vec 3 3 3) ]) (sphere (vec 0 0 0) 32)) t_shiny_red
   , transform (portal 5 2 (1/3)) [ rotate vy (deg 8)
                                  , translate (vec (-3) 0.5 (-5))]
   , transform (tex (sphere (Vec (-2.3) 0.3 (4.2)) 1.7) (t_uniform $ Refract 0.35 0.8 1.5)) [scale (vec 1 0.4 1) ]
   ]


-- some textures
m_shiny_white :: M
m_shiny_white = (Surface c_white 1 0.2 0.8 0.4 10 False)

m_shiny_red = (Surface c_red 1 0.2 0.8 0.4 10 False)

t_shiny_white :: T
t_shiny_white _ _ = m_shiny_white

t_shiny_red _ _ = m_shiny_red

m_dull_gray :: M
m_dull_gray = (Surface (Color 0.4 0.3 0.35) 1 0.2 0.8 0 0 False)

t_mottled _ (RayHit _ pos norm _ _ _ _) =
 --let scale = (stripe (Vec 1 1 1) sine_wave) pos
 let scale = perlin (vscale pos 3)
 in if scale < 0 then error "perlin under range"
    else if scale > 1 
         then error "perlin over range"
         else Blend m_mirror (m_matte (Color 0.15 0.3 0.5)) scale

--shouldn't happen
t_mottled _ RayMiss = m_shiny_white

t_stripe :: T
t_stripe _ (RayHit _ pos norm _ _ _ _) =
 let scale = (stripe (Vec 4 8 5) triangle_wave) pos
 in if scale < 0 then error "perlin under range"
    else if scale > 1 
         then error "perlin over range"
         else Blend m_shiny_white m_dull_gray scale

--shouldn't happen
t_stripe _ RayMiss = m_shiny_white 

m_matte :: Color -> M
m_matte c = (Surface c 1 0.2 1 0 0 False)

t_matte :: Color -> T
t_matte c = 
 (\_ _ -> m_matte c)

m_mirror = (Reflect 0.8)
t_mirror = 
 (\_ _ -> m_mirror)

c_sky = (Color 0.4 0.5 0.8)

scn :: IO Scene
scn = return (geom'', lights, cust_cam, materialShader)

