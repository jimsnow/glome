{-
Copyright (c) 2008 Jim Snow
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Glome.Bih (bih) where
import Data.Glome.Vec
import Data.Glome.Solid

-- for specialization
import Data.Glome.Triangle(Triangle, TriangleNorm) 
import Data.Glome.Sphere(Sphere)

import Data.List hiding (group) -- for "partition"
import Debug.Trace
import Control.DeepSeq(NFData, rnf)
import Control.Monad.Par

-- Bounding Interval Heirarchy
-- http://en.wikipedia.org/wiki/Bounding_interval_hierarchy

data Solid s t m  => Bih s t m =
  Bih {bihbb :: Bbox, bihroot :: BihNode s t m} deriving Show

-- Branch fields are left max, right min, axis, left branch, right branch
data Solid s t m => BihNode s t m =
  BihLeaf [s] |
  BihBranch !Flt !Flt !Int (BihNode s t m) (BihNode s t m) deriving Show


instance Solid s t m => NFData (BihNode s t m) where
  rnf (BihBranch _ _ _ l r) = rnf l `seq` rnf r `seq` ()
  rnf _ = ()

-- bih construction
-- create a leaf node from a list of objects
-- we use "group" so we can treat a bunch of objects as a single object
build_leaf :: Solid s t m => [(Bbox, s)] -> BihNode s t m
build_leaf objs =
 BihLeaf (map snd objs)

-- return surface area of a bounding box that encloses bounding boxes
-- divided by the surface area of the nodebox

-- this doesn't seem to be much of a win

optimality :: [(Bbox, SolidItem t m)] -> Bbox -> Flt
optimality objs bb =
 let bbsurf = bbsa bb
     go [] accbb = accbb
     go ((obb,_):xs) accbb = go xs (bbjoin obb accbb)
     obbsurf = bbsa $! bboverlap (go objs empty_bbox) bb
 in
  obbsurf / bbsurf

-- tuning parameter that controls threshold for separating
-- large objects from small objects instead of usual left/right
-- sorting 

-- was 0.3

max_bih_sa = 0.4 :: Flt

lenMax xs max =
  go xs 0
  where
    go [] n = n
    go (x:xs) n =
      if n >= max
      then max
      else go xs (n+1) 

-- Recursive constructor, it looks like quicksort if you squint hard enough.
-- We split along the splitbox's axis of greatest extent, then sort objects
-- to one side or the other (they can overlap the center), then construct the
-- branch node and recurse.

-- I added a nonstandard heuristic: if there's a few very large objects and a lot
-- of small ones, we create one branch with big objects and the other with small
-- objects, instead of sorting by location.

{-
build_rec' :: Solid s t m => [(Bbox, s)] -> Bbox -> Bbox -> Int -> BihNode s t m
build_rec' objs nodebox@(Bbox nodeboxp1 nodeboxp2) splitbox@(Bbox splitboxp1 splitboxp2) depth = 

 if (lenMax objs 3 <= 2) -- && (optimality objs nodebox) > 0.2
 then build_leaf objs
 else
  let axis  = vmaxaxis (vsub splitboxp2 splitboxp1)
      bbmin = va splitboxp1 axis
      bbmax = va splitboxp2 axis
      candidate = (bbmin + bbmax) * 0.5
  in
   if candidate > (va nodeboxp2 axis) then
    build_rec objs nodebox 
              (Bbox splitboxp1 (vset splitboxp2 axis candidate)) 
              depth
   else
    if candidate < (va nodeboxp1 axis) then
     build_rec objs nodebox (
               Bbox (vset splitboxp1 axis candidate) splitboxp2) 
               depth
    else
     -- not sure if this is a big win
     let nbsa = bbsa nodebox
         (big,small) = partition (\ (bb,_) -> 
                                   (bbsa bb) > (nbsa * max_bih_sa)) objs
     in 
      if (not $ null big) && ((length big) < ((length small)*2))
      then (BihBranch (va nodeboxp2 0) (va nodeboxp1 0) 0
                      (build_rec big nodebox splitbox (depth+1))
                      (build_rec small nodebox splitbox (depth+1)) )
      else
       let (l,r) = partition (\((Bbox bbp1 bbp2),_)-> 
                               (((va bbp1 axis)+(va bbp2 axis))*0.5) 
                                 < candidate ) objs
           lmax = foldl' fmax (-infinity) (map (\((Bbox _ p2),_) -> va p2 axis) l)
           rmin = foldl' fmin   infinity  (map (\((Bbox p1 _),_) -> va p1 axis) r)
           (lsplit,rsplit) = bbsplit splitbox axis candidate
           lnb  = (Bbox nodeboxp1 (vset nodeboxp2 axis lmax))
           rnb  = (Bbox (vset nodeboxp1 axis rmin) nodeboxp2)
       in
        -- stop if there's no progress being made
        if ((null l) && (rmin <= bbmin)) ||
           ((null r) && (lmax >= bbmax))
        then build_leaf objs
        else
         (BihBranch (lmax+delta) (rmin-delta) axis
                    (build_rec l lnb lsplit (depth+1))
                    (build_rec r rnb rsplit (depth+1)) )
-}

bbmid (Bbox p1 p2) = (vscale (vadd p1 p2) 0.5)

{-
build_rec :: Solid s t m => [(Bbox, s)] -> Bbox -> Int -> Int -> BihNode s t m
build_rec objs bb@(Bbox bbp1 bbp2) depth objcount = 

 if (objcount <= 4)
 then build_leaf objs
 else
  let axis  = vmaxaxis (vsub bbp1 bbp2)
      bbmin = va bbp1 axis
      bbmax = va bbp2 axis
      candidate = (bbmin + bbmax) * 0.5
      sa = bbsa' bb

      costmod = if depth < 10
                then 1 
                else if depth < 20
                     then 1.2
                     else 1.5

      (l,r) = partition (\(bb,_) -> va (bbmid bb) axis < candidate) objs
      
      lmax = foldl' fmax (-infinity) (map (\((Bbox _ p2),_) -> va p2 axis) l)
      rmin = foldl' fmin   infinity  (map (\((Bbox p1 _),_) -> va p1 axis) r)
      
      lcount = length l
      rcount = length r

      lbb  = Bbox bbp1 (vset bbp2 axis lmax)
      rbb  = Bbox (vset bbp1 axis rmin) bbp2

      origcost = sa * (fromIntegral objcount)
      lcost = ((fromIntegral lcount) * (bbsa' lbb)) * costmod
      rcost = ((fromIntegral rcount) * (bbsa' rbb)) * costmod
      newcost = lcost + rcost
  in
    if newcost > origcost
    then build_leaf objs
    else
      (BihBranch (lmax+delta) (rmin-delta) axis
                 (build_rec l lbb (depth+1) lcount)
                 (build_rec r rbb (depth+1) rcount) )
-}


bbsa' bb = max 0 $ bbsa bb

-- Try splitting on 3 axes and big/small, go with the least cost.
build_rec :: Solid s t m => [(Bbox, s)] -> Bbox -> Vec -> Int -> Int -> BihNode s t m
build_rec objs bb@(Bbox bbp1 bbp2) mid depth objcount = 

 if (objcount <= 3)
 then build_leaf objs
 else
  let Vec xmid ymid zmid = mid
      sa = bbsa' bb

      (lx,rx) = partition (\(bb,_) -> x (bbmid bb) < xmid) objs
      (ly,ry) = partition (\(bb,_) -> y (bbmid bb) < ymid) objs
      (lz,rz) = partition (\(bb,_) -> z (bbmid bb) < zmid) objs
      (big,small) = partition (\(bb,_) -> bbsa' bb > sa * 0.4) objs

      lxmax = foldl' fmax (-infinity) (map (\((Bbox _ p2),_) -> x p2) lx)
      rxmin = foldl' fmin   infinity  (map (\((Bbox p1 _),_) -> x p1) rx)
      lymax = foldl' fmax (-infinity) (map (\((Bbox _ p2),_) -> y p2) ly)
      rymin = foldl' fmin   infinity  (map (\((Bbox p1 _),_) -> y p1) ry)
      lzmax = foldl' fmax (-infinity) (map (\((Bbox _ p2),_) -> z p2) lz)
      rzmin = foldl' fmin   infinity  (map (\((Bbox p1 _),_) -> z p1) rz)
      lbmax = foldl' fmax (-infinity) (map (\((Bbox _ p2),_) -> x p2) big)
      rbmin = foldl' fmin   infinity  (map (\((Bbox p1 _),_) -> x p1) small)

      lxcount = length lx
      rxcount = length rx
      lycount = length ly
      rycount = length ry
      lzcount = length lz
      rzcount = length rz
      lbcount = length big
      rbcount = length small

      lxbb  = Bbox bbp1 (vset bbp2 0 lxmax)
      rxbb  = Bbox (vset bbp1 0 rxmin) bbp2
      lybb  = Bbox bbp1 (vset bbp2 1 lymax)
      rybb  = Bbox (vset bbp1 1 rymin) bbp2
      lzbb  = Bbox bbp1 (vset bbp2 2 lzmax)
      rzbb  = Bbox (vset bbp1 2 rzmin) bbp2
      lbbb  = Bbox bbp1 (vset bbp2 0 lbmax)
      rbbb  = Bbox (vset bbp1 0 rbmin) bbp2

      costx = ((bbsa' lxbb * fromIntegral lxcount) + (bbsa' rxbb * fromIntegral rxcount)) * 1.1
      costy = ((bbsa' lybb * fromIntegral lycount) + (bbsa' rybb * fromIntegral rycount)) * 1.1
      costz = ((bbsa' lzbb * fromIntegral lzcount) + (bbsa' rzbb * fromIntegral rzcount)) * 1.1
      costb = ((bbsa' lbbb * fromIntegral lbcount) + (bbsa' rbbb * fromIntegral rbcount)) * 1.2

      costorig = sa * fromIntegral objcount
      nextd = depth+1

      build_branch loff roff axis lobjs lbb lcount robjs rbb rcount =
        let lbbmid = bbmid lbb
            rbbmid = bbmid rbb
            --lbbmid = vset mid axis ((va bbp1 axis + va mid axis)*0.5)
            --rbbmid = vset mid axis ((va bbp2 axis + va mid axis)*0.5)
        in
          if depth > 5
          then BihBranch loff roff axis (build_rec lobjs lbb lbbmid nextd lcount) (build_rec robjs rbb rbbmid nextd rcount)
          else
            runPar $ do
              ljob <- spawnP (build_rec lobjs lbb lbbmid nextd lcount)
              rjob <- spawnP (build_rec robjs rbb rbbmid nextd rcount)
              l <- get ljob
              r <- get rjob
              return $ BihBranch loff roff axis l r
  in
     if costorig < costx && costorig < costy && costorig < costz && costorig < costb
     then build_leaf objs
     else
       if costx < costy && costx < costz && costx < costb
       then build_branch (lxmax+delta) (rxmin-delta) 0 lx lxbb lxcount rx rxbb rxcount
       else if costy < costz && costy < costb
            then build_branch (lymax+delta) (rymin-delta) 1 ly lybb lycount ry rybb rycount
            else if costy < costb
                 then build_branch (lzmax+delta) (rzmin-delta) 2 lz lzbb lzcount rz rzbb rzcount
                 else build_branch (lbmax+delta) (rbmin-delta) 0 big lbbb lbcount small rbbb rbcount


build_branch' loff roff axis lb rb depth 
 | depth > 6 = BihBranch loff roff axis lb rb
 | otherwise =
    runPar $ do
      ljob <- spawnP lb
      rjob <- spawnP rb
      l <- get ljob
      r <- get rjob
      return $ BihBranch loff roff axis l r

-- | The bih constructor creates a Bounding Interval Heirarchy
-- from a list of primitives.  BIH is a type of data structure
-- that groups primitives into a heirarchy of bounding objects,
-- so that a ray need not be tested against every single
-- primitive.  This can make the difference betweeen a rendering
-- job that takes days or seconds.  BIH usually performs a little
-- worse than a SAH-based KD-tree, but construction time is much
-- better.
--
-- See http://en.wikipedia.org/wiki/Bounding_interval_hierarchy

bih :: Solid s t m => [s] -> SolidItem t m
bih [] = SolidItem Void
-- bih (sld:[]) = sld  -- sometimes we'd like to be able to use a
                       -- single object bih just for its aabb
bih slds =
 let objs = map (\x -> ((bound x),x)) slds -- (flatten_group slds)
     bb   = foldl' bbjoin empty_bbox (map (\(b,_)->b) objs)
     root = build_rec objs bb (bbmid bb) 0 (length slds)
     (Bbox (Vec p1x p1y p1z) (Vec p2x p2y p2z)) = bb
 in
  if p1x == (-infinity) || p1y == (-infinity) || p1z == (-infinity) ||
     p2x == infinity    || p2y == infinity    || p2z == infinity
  then
   error $ "bih: infinite bounding box " ++ (show objs)
  else
   SolidItem (Bih bb root)


miss :: Rayint tag mat
miss = RayMiss

-- Optimized traversal.  There's an allocation happening somewhere in here
-- that I haven't been able to eradicate.
rayint_bih :: Solid s tag mat => Bih s tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_bih (Bih !bb !root) !r !d t tags =
  let (# near, far #) =  bbclip_ub r bb
      (# ox, oy, oz, dx, dy, dz #) = ray_ub r
      dirrx = 1/dx
      dirry = 1/dy
      dirrz = 1/dz
      traverse (BihLeaf !s) !near !far = rayint s r far t tags
      traverse (BihBranch !lsplit !rsplit !axis l r) !near !far =
        let (# !dirr, !o #) =  (case axis of 0 -> (# dirrx, ox #)
                                             1 -> (# dirry, oy #)
                                             2 -> (# dirrz, oz #)) 
            !dl   = (lsplit - o) * dirr
            !dr   = (rsplit - o) * dirr
        in  
            if near > far 
            then miss
            else
             if dirr > 0
             then 
              nearest
               (if near < dl
                then traverse l near (fmin dl far)
                else miss)
               (if dr < far
                then traverse r (fmax dr near) far
                else miss)
             else
              nearest
               (if near < dr
                then traverse r near (fmin dr far)
                else miss)
               (if dl < far
                then traverse l (fmax dl near) far
                else miss)
  in
     traverse root near (fmin d far)

{-# SPECIALIZE rayint_bih :: Bih (Triangle tag mat) tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat #-}
{-# SPECIALIZE rayint_bih :: Bih (TriangleNorm tag mat) tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat #-}
{-# SPECIALIZE rayint_bih :: Bih (Sphere tag mat) tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat #-}

{-# SPECIALIZE rayint_bih :: Bih (SolidItem tag mat) tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat #-}

-- Ray traversal with debug counter.  The counter gets incremented
-- when we hit a box.
rayint_debug_bih :: Solid s tag mat => Bih s tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> (Rayint tag mat, Int) 
rayint_debug_bih (Bih bb root) r@(Ray orig dir) d t tags =
 let !dir_rcp = vrcp dir
     Interval !near !far = bbclip r bb
     traverse (BihLeaf !s) !near !far = rayint_debug s r (fmin d far) t tags
     traverse (BihBranch !lsplit !rsplit !axis !l !r) near far =
       let dirr = va dir_rcp axis
           o    = va orig axis
           dl   = (lsplit - o) * dirr
           dr   = (rsplit - o) * dirr
       in 
         debug_wrap 
          (if near > far 
           then (RayMiss,0)
           else
            if dirr > 0
            then 
             (nearest_debug
              (if near < dl
               then traverse l near (fmin dl far)
               else (RayMiss,0))
              (if dr < far
               then traverse r (fmax dr near) far
               else (RayMiss,0)))
            else
             (nearest_debug
              (if near < dr
               then traverse r near (fmin dr far)
               else (RayMiss,0))
              (if dl < far
               then traverse l (fmax dl near) far
               else (RayMiss,0))))
          1 -- increment the debug value for every box we hit
 in
  traverse root near far

-- This is unwieldy, but the performance gains
-- sometimes make it worthwhile.  By testing 4 rays against 
-- each cell, we (theoretically) do ~1/4 the 
-- memory accesses. 

-- This originally made a big difference, but after switching
-- everything to typeclasses, it doesn't perform any better
-- than regular traversal.

-- One simplifying assumption we make that adds a 
-- little bit of overhead:  If one ray hits a cell, 
-- we act as though they all do.  For that reason,
-- this only works well with coherent rays.

packetint_bih :: Solid s tag mat => Bih s tag mat -> Ray -> Ray -> Ray -> Ray -> Flt -> [Texture tag mat] -> [tag] -> PacketResult tag mat
packetint_bih bih@(Bih bb root) 
              !r1@(Ray orig1 dir1) 
              !r2@(Ray orig2 dir2) 
              !r3@(Ray orig3 dir3) 
              !r4@(Ray orig4 dir4) !d t tags =
 let !dir_rcp1 = vrcp dir1
     !dir_rcp2 = vrcp dir2
     !dir_rcp3 = vrcp dir3
     !dir_rcp4 = vrcp dir4
 in
  -- We want all the ray components to have
  -- at least the same sign.
  if not $ veqsign dir_rcp1 dir_rcp2 &&
           veqsign dir_rcp1 dir_rcp3 &&
           veqsign dir_rcp1 dir_rcp4
  then
   PacketResult (rayint bih r1 d t tags)
                (rayint bih r2 d t tags)
                (rayint bih r3 d t tags)
                (rayint bih r4 d t tags)
  else 
   let Interval !near1 !far1 = bbclip r1 bb
       Interval !near2 !far2 = bbclip r2 bb
       Interval !near3 !far3 = bbclip r3 bb
       Interval !near4 !far4 = bbclip r4 bb

       !near = fmin4 near1 near2 near3 near4
       !far =  fmax4 far1  far2  far3  far4

       traverse (BihLeaf !s) !near !far = packetint s r1 r2 r3 r4 (fmin d far) t tags
       traverse (BihBranch !lsplit !rsplit !axis !l !r) !near !far =
           if near > far 
           then packetmiss
           else
            let dirr1 = va dir_rcp1 axis
                dirr2 = va dir_rcp2 axis
                dirr3 = va dir_rcp3 axis
                dirr4 = va dir_rcp4 axis
                     
                o1    = va orig1 axis
                o2    = va orig2 axis
                o3    = va orig3 axis
                o4    = va orig4 axis

                dl1   = (lsplit - o1) * dirr1
                dl2   = (lsplit - o2) * dirr2
                dl3   = (lsplit - o3) * dirr3
                dl4   = (lsplit - o4) * dirr4

                dr1   = (rsplit - o1) * dirr1
                dr2   = (rsplit - o2) * dirr2
                dr3   = (rsplit - o3) * dirr3
                dr4   = (rsplit - o4) * dirr4

            in  
             if dirr1 > 0  -- true for all, since signs match
             then 
              let dl = fmax4 dl1 dl2 dl3 dl4
                  dr = fmin4 dr1 dr2 dr3 dr4
              in
               (nearest_packetresult
                (if near < dl
                 then traverse l near (fmin dl far)
                 else packetmiss)
                (if dr < far
                 then traverse r (fmax dr near) far
                 else packetmiss))
             else
              let dl = fmin4 dl1 dl2 dl3 dl4
                  dr = fmax4 dr1 dr2 dr3 dr4
              in
               (nearest_packetresult
                (if near < dr
                 then traverse r near (fmin dr far)
                 else packetmiss)
                (if dl < far
                 then traverse l (fmax dl near) far
                 else packetmiss))
   in
    traverse root near far

shadow_bih :: Solid s t m => Bih s t m -> Ray -> Flt -> Bool
shadow_bih (Bih bb root) r@(Ray orig@(Vec ox oy oz) dir@(Vec dx dy dz)) d =
 let -- !dir_rcp = vrcp dir
     (# near, far' #) = bbclip_ub r bb
     !far = fmin d far'
     traverse (BihLeaf !s) !near !far = shadow s r (fmin d far)
     traverse (BihBranch !lsplit !rsplit !axis l r) !near !far =
      let (# !dirr, !o #) = {-# SCC bih_shadow_case #-} (case axis of 0 -> (# 1/dx, ox #)
                                                                      1 -> (# 1/dy, oy #)
                                                                      2 -> (# 1/dz, oz #)) 
          !dl = (lsplit - o) * dirr
          !dr = (rsplit - o) * dirr
      in  
          if near > far 
          then False
          else
           if dirr > 0
           then
            ((if near < dl
              then traverse l near $! (fmin dl far)
              else False) 
             ||
             (if dr < far
              then traverse r (fmax dr near) far
              else False))
           else
            ((if near < dr
              then traverse r near $! (fmin dr far)
              else False)
             ||
             (if dl < far
              then traverse l (fmax dl near) far
              else False))

 in traverse root near far

-- Inside/outside test; essentially a point traversal.
-- We test if the point is inside any of the objects contained in
-- the bih.

inside_bih :: Solid s t m => Bih s t m -> Vec -> Bool
inside_bih (Bih (Bbox (Vec !x1 !y1 !z1) (Vec !x2 !y2 !z2)) root) pt@(Vec !x !y !z) =
 let traverse (BihLeaf !s) = inside s pt
     traverse (BihBranch !lsplit !rsplit !axis !l !r) =
       let o = va pt axis
       in (if o < lsplit
           then (traverse l)
           else False) 
          ||
          (if o > rsplit 
           then (traverse r)
           else False)
 in
  (x > x1) && (x < x2) && 
  (y > y1) && (y < y2) && 
  (z > z1) && (z < z2) && (traverse root)

get_metainfo_bih (Bih (Bbox (Vec !x1 !y1 !z1) (Vec !x2 !y2 !z2)) root) pt@(Vec !x !y !z) =
 let traverse (BihLeaf !s) = get_metainfo s pt
     traverse (BihBranch !lsplit !rsplit !axis !l !r) =
       let o = va pt axis
       in (if o < lsplit
           then (traverse l)
           else ([],[])) 
          `paircat`
          (if o > rsplit 
           then (traverse r)
           else ([],[]))
 in
  if (x > x1) && (x < x2) && 
     (y > y1) && (y < y2) && 
     (z > z1) && (z < z2)
  then
    (traverse root)
  else
    ([],[])

-- We already have a bounding box computed.
bound_bih :: Solid s t m => Bih s t m -> Bbox
bound_bih (Bih bb root) = bb

primcount_bih :: Solid s t m => Bih s t m -> Pcount
primcount_bih (Bih bb root) = pcadd (bihcount root) pcsinglebound
 where bihcount (BihLeaf s) = primcount s
       bihcount (BihBranch _ _ _ l r) = 
        pcadd (pcadd (bihcount l) (bihcount r)) pcsinglebound

instance Solid s t m => Solid (Bih s t m) t m where
 rayint = rayint_bih
 rayint_debug = rayint_debug_bih
 packetint = packetint_bih
 shadow = shadow_bih
 inside = inside_bih
 bound = bound_bih
 primcount = primcount_bih
 get_metainfo = get_metainfo_bih
