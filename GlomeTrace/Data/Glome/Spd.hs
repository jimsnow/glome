{-# LANGUAGE FlexibleInstances #-}

module Data.Glome.Spd where
import Data.Glome.Scene

-- NFF file format description:
-- http://tog.acm.org/resources/SPD/NFF.TXT

-- this would probably be much shorter if I used scanf instead of lex

lexignore s =
 let t = lex s 
 in
  case t of 
   [] -> [] -- newline
   [("",s1)] -> lexcr s1
   [(_,s1)] -> lexignore s1 

lexcr s = 
 let t = lex s
 in 
  case t of
   [(s1,[])] -> t
   [("",s1)] -> lexcr s1
   [("#",s1)] -> lexignore s1
   _ -> t

data BgColor = BgColor(Color)

readsSpdVec :: ReadS Vec
readsSpdVec s = [((Vec x y z),s3) | (x,s1) <- reads s  :: [(Flt,String)], 
                                    (y,s2) <- reads s1 :: [(Flt,String)],
                                    (z,s3) <- reads s2 :: [(Flt,String)] ]
instance Read Vec where
 readsPrec _ = readsSpdVec

readsSpdVecNorm :: ReadS (Vec,Vec)
readsSpdVecNorm s = [(((Vec x y z),(Vec nx ny nz)),s6) 
                     | (x,s1) <- reads s  :: [(Flt,String)], 
                       (y,s2) <- reads s1 :: [(Flt,String)],
                       (z,s3) <- reads s2 :: [(Flt,String)],
                       (nx,s4) <- reads s3  :: [(Flt,String)],
                       (ny,s5) <- reads s4  :: [(Flt,String)],
                       (nz,s6) <- reads s5  :: [(Flt,String)] ]

instance Read (Vec,Vec) where
 readsPrec _ = readsSpdVecNorm


-- if this seems intuitive, there's something wrong with you
readsSpdVecs :: ReadS [Vec]
readsSpdVecs s =
 let parses = reads s :: [(Vec,String)]
 in
 if null parses
 then [([],s)]
 else
  let (v,rest) = head parses
      (vs,returns) = head (readsSpdVecs rest)
  in [((v:vs),returns)]

instance Read [Vec] where
 readsPrec _ = readsSpdVecs

readsSpdVecsNorms :: ReadS [(Vec,Vec)]
readsSpdVecsNorms s = 
 let parses = readsSpdVecNorm s :: [((Vec,Vec),String)]
 in
 if null parses
 then [([],s)]
 else
  let (v,rest) = head parses
      (vs,returns) = head (readsSpdVecsNorms rest)
  in [((v:vs),returns)]

instance Read [(Vec,Vec)] where
 readsPrec _ = readsSpdVecsNorms

{- "v"
   "from" Fx Fy Fz
   "at" Ax Ay Az
   "up" Ux Uy Uz
   "angle" angle
   "hither" hither
   "resolution" xres yres -}
readsSpdCam :: ReadS Camera
readsSpdCam s = [ (camera from at up angle,s14) | ("v", s1)      <- lexcr s,
                                                  ("from", s2)   <- lexcr s1,
                                                  (from, s3)     <- reads s2 :: [(Vec,String)],
                                                  ("at", s4)     <- lexcr s3,
                                                  (at, s5)       <- reads s4 :: [(Vec,String)],
                                                  ("up", s6)     <- lexcr s5,
                                                  (up, s7)       <- reads s6 :: [(Vec,String)],
                                                  ("angle", s8)  <- lexcr s7,
                                                  (angle, s9)    <- reads s8 :: [(Flt,String)],
                                                  ("hither", s10)<- lexcr s9,
                                                  (_,s11)        <- lexcr s10,
                                                  ("resolution", s12) <- lexcr s11,
                                                  (_, s13)       <- lexcr s12,
                                                  (_, s14)       <- lexcr s13 ]
instance Read Camera where
 readsPrec _ = readsSpdCam



readsSpdClr :: ReadS Color
readsSpdClr s = [((Color r g b), s3) | (r, s1)  <- reads s  :: [(Flt,String)],
                                       (g, s2)  <- reads s1 :: [(Flt,String)],
                                       (b, s3)  <- reads s2 :: [(Flt,String)] ]
instance Read Color where
 readsPrec _ = readsSpdClr


-- "b" R G B
readsSpdBackground :: ReadS BgColor
readsSpdBackground s = [((BgColor clr), s2) | ("b", s1) <- lexcr s,
                                     (clr, s2) <- reads s1 :: [(Color,String)] ]
instance Read BgColor where
 readsPrec _ = readsSpdBackground


-- "l" X Y Z [R G B]
readsSpdLight :: ReadS Light
readsSpdLight s = [((light pos clr),s3) | ("l", s1) <- lexcr s,
                                          (pos, s2) <- reads s1 :: [(Vec,String)],
                                          (clr, s3) <- reads s2 :: [(Color,String)] ]
                  ++
                  [((light pos (Color 1 1 1)),s2) | ("l", s1) <- lexcr s,
                                                    (pos, s2) <- reads s1 :: [(Vec,String)] ]
instance Read Light where
 readsPrec _ = readsSpdLight

-- "f" red green blue Kd Ks Shine T index_of_refraction
-- data Material = Material {clr :: Color, reflect, refract, ior, kd, ks, shine :: Flt}
readsSpdFill :: ReadS Texture
readsSpdFill s = [(\ri->Material clr ks (1-trans) ior kd 0.5 shine, s7) | ("f", s1)    <- lexcr s,
                                    (clr, s2)    <- reads s1 :: [(Color,String)],
                                    (kd, s3)     <- reads s2 :: [(Flt,String)],
                                    (ks, s4)     <- reads s3 :: [(Flt,String)],
                                    (shine, s5)  <- reads s4 :: [(Flt,String)],
                                    (trans, s6)  <- reads s5 :: [(Flt,String)],
                                    (ior, s7)    <- reads s6 :: [(Flt,String)] ]

instance Read (Rayint -> Material) where
 readsPrec _ = readsSpdFill


-- "s" center.x center.y center.z radius

-- "c"
-- base.x base.y base.z base_radius
-- apex.x apex.y apex.z apex_radius

-- "p" total_vertices
-- vert1.x vert1.y vert1.z
-- [etc. for total_vertices vertices]

readsSpdSolid :: ReadS SolidItem
readsSpdSolid s = [((sphere center radius),s3) | ("s", s1) <- lexcr s,
                                                 (center,s2) <- reads s1 :: [(Vec,String)],
                                                 (radius,s3) <- reads s2 :: [(Flt,String)] ]
                  ++
                  [((cone e1 r1 e2 r2),s5) | ("c",s1) <- lexcr s,
                                             (e1,s2)  <- reads s1 :: [(Vec,String)],
                                             (r1,s3)  <- reads s2 :: [(Flt,String)],
                                             (e2,s4)  <- reads s3 :: [(Vec,String)],
                                             (r2,s5)  <- reads s4 :: [(Flt,String)] ]
                  ++
                  [(group (triangles verts),s3) | ("p",s1) <- lexcr s,
                                                  (n,s2) <- reads s1 :: [(Int,String)],
                                                  (verts,s3) <- readsSpdVecs s2 :: [([Vec],String)] ]
                  ++
                  [(group (trianglesnorms (vns)),s3) | ("pp",s1) <- lexcr s,
                                                       (n,s2) <- reads s1 :: [(Int,String)],
                                                       (vns,s3) <- readsSpdVecsNorms s2 :: [([(Vec,Vec)],String)] ]
                  {- ++
                  [(tex(Void,t),s1) | (t,s1) <- reads s :: [(Texture,String)]] -}


-- instance Read Solid where
-- readsPrec _ = readsSpdSolid


-- same as readSpdVecs, just different types
readsSpdPrims :: ReadS [SolidItem]
readsSpdPrims s =
 let parses = readsSpdSolid s :: [(SolidItem,String)]
 in
 if null parses
 then [([],s)]
 else
  let (v,rest) = head parses
      (vs,returns) = head (readsSpdPrims rest)
  in [((v:vs),returns)]

instance Read [SolidItem] where
 readsPrec _ = readsSpdPrims


readsSpdTextureGroup :: ReadS SolidItem
readsSpdTextureGroup s =
 [((tex (bih prims) t),s2) | (t,s1)     <- reads s :: [(Texture,String)],
                               (prims,s2) <- readsSpdPrims s1 :: [([SolidItem],String)] ]
 
instance Read SolidItem where
 readsPrec _ = readsSpdTextureGroup

accum_rss :: [Camera] -> [Light] -> [SolidItem] -> [BgColor] -> String -> ([Camera],[Light],[SolidItem],[BgColor],String)
accum_rss cams lights prims background s = 
  if null s
   then (cams,lights,prims,background,s)
  else
   let cam = reads s :: [(Camera,String)]
       sld = reads s :: [(SolidItem,String)]
       lit = reads s :: [(Light,String)]
       bgc = reads s :: [(BgColor,String)]
   in
     if not $ null cam
     then
       let (c1,s1) = head cam
      in accum_rss (c1:cams) lights prims background s1
     else
      if not $ null sld
      then
       let (s2,s1) = head sld 
       in  accum_rss cams lights (s2:prims) background s1
      else
       if not $ null lit
       then
        let (l1,s1) = head lit
        in accum_rss cams (l1:lights) prims background s1
       else
        if not $ null bgc
        then
         let (b1,s1) = head bgc
         in accum_rss cams lights prims (b1:background) s1
        else
         (cams,lights,prims,background,s)

readsSpdScene :: ReadS Scene
readsSpdScene s = 
  let ((cam:cams),lights,prims,(BgColor(bgc):bgcs),s1) = accum_rss [] [] [] [] s
  in [((scene (bih prims) lights cam t_white bgc),s1)]

-- | Read instance for scenes described in the Neutral File Format
-- (NFF) used by SPD, a collection of standard benchmark scenes put
-- together by Eric Haines.  We support most NFF features, but not
-- all.
instance Read Scene where
 readsPrec _ = readsSpdScene
