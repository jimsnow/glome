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

module Data.Glome.Bih (bih) where
import Data.Glome.Vec
import Data.Glome.Solid
import Data.List hiding (group) -- for "partition"


-- Bounding Interval Heirarchy
-- http://en.wikipedia.org/wiki/Bounding_interval_hierarchy

data Bih t m = Bih {bihbb :: Bbox, bihroot :: BihNode t m} deriving Show
data BihNode t m = BihLeaf !(SolidItem t m) 
             | BihBranch {lmax :: !Flt, rmin :: !Flt, ax :: !Int, 
                          l :: BihNode t m, r :: BihNode t m} deriving Show

-- bih construction
-- create a leaf node from a list of objects
-- we use "group" so we can treat a bunch of objects as a single object
build_leaf :: [(Bbox, SolidItem t m)] -> BihNode t m
build_leaf objs =
 BihLeaf (group (map snd objs))

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

-- Recursive constructor, it looks like quicksort if you squint hard enough.
-- We split along the splitbox's axis of greatest extent, then sort objects
-- to one side or the other (they can overlap the center), then construct the
-- branch node and recurse.

-- I added a nonstandard heuristic: if there's a few very large objects and a lot
-- of small ones, we create one branch with big objects and the other with small
-- objects, instead of sorting by location.

build_rec :: [(Bbox, SolidItem t m)] -> Bbox -> Bbox -> Int -> BihNode t m
build_rec objs nodebox@(Bbox nodeboxp1 nodeboxp2) splitbox@(Bbox splitboxp1 splitboxp2) depth = 

 if (length (take 3 objs) < 2) -- && (optimality objs nodebox) > 0.2
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
           lmax = foldl fmax (-infinity) (map (\((Bbox _ p2),_) -> va p2 axis) l)
           rmin = foldl fmin   infinity  (map (\((Bbox p1 _),_) -> va p1 axis) r)
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

bih :: [SolidItem t m] -> SolidItem t m
bih [] = SolidItem Void
-- bih (sld:[]) = sld  -- sometimes we'd like to be able to use a
                       -- single object bih just for its aabb
bih slds =
 let objs = map (\x -> ((bound x),x)) (flatten_group slds)
     bb   = foldl bbjoin empty_bbox (map (\(b,_)->b) objs)
     root = build_rec objs bb bb 0
     (Bbox (Vec p1x p1y p1z) (Vec p2x p2y p2z)) = bb
 in
  if p1x == (-infinity) || p1y == (-infinity) || p1z == (-infinity) ||
     p2x == infinity    || p2y == infinity    || p2z == infinity
  then
   error $ "bih: infinite bounding box " ++ (show objs)
  else
   SolidItem (Bih bb root)

-- Standard ray traversal.
rayint_bih' :: Bih tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_bih' (Bih bb root) !r@(Ray orig dir) !d t tags =
 let !dir_rcp = vrcp dir
     Interval !near !far = bbclip r bb
     traverse (BihLeaf !s) !near !far = rayint s r (fmin d far) t tags
     traverse (BihBranch !lsplit !rsplit !axis !l !r) near far =
       let !dirr = va dir_rcp axis
           !o    = va orig axis
           !dl   = (lsplit - o) * dirr
           !dr   = (rsplit - o) * dirr
       in  
           if near > far 
           then RayMiss
           else
            if dirr > 0
            then 
             (nearest
              (if near < dl
               then traverse l near (fmin dl far)
               else RayMiss)
              (if dr < far
               then traverse r (fmax dr near) far
               else RayMiss))
            else
             (nearest
              (if near < dr
               then traverse r near (fmin dr far)
               else RayMiss)
              (if dl < far
               then traverse l (fmax dl near) far
               else RayMiss))
 in
  traverse root near far

miss :: Rayint tag mat
miss = RayMiss

-- Optimized traversal.  There's an allocation happening somewhere in here
-- that I haven't been able to eradicate.
rayint_bih :: Bih tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_bih (Bih !bb !root) !r !d t tags =
  let (# near, far #) = {-# SCC bih_rayint_clip #-} bbclip_ub r bb
      (# ox, oy, oz, dx, dy, dz #) = {-# SCC bih_rayint_ray #-} ray_ub r
      traverse (BihLeaf !s) !near !far = {-# SCC bih_rayint_leaf #-} rayint s r far t tags
      traverse (BihBranch !lsplit !rsplit !axis l r) !near !far =
        {-# SCC bih_rayint_branch #-}
        let (# !dirr, !o #) = {-# SCC bih_rayint_case #-} (case axis of 0 -> (# 1/dx, ox #)
                                                                        1 -> (# 1/dy, oy #)
                                                                        2 -> (# 1/dz, oz #)) 
            !dl   = (lsplit - o) * dirr
            !dr   = (rsplit - o) * dirr
        in  
            if near > far 
            then miss
            else
             if dirr > 0
             then
              {-# SCC bih_rayint_pos #-} 
              nearest
               (if near < dl
                then (case fmin dl far of far' -> traverse l near far')
                else miss)
               (if dr < far
                then (case fmax dr near of near' -> traverse r near' far)
                else miss)
             else
              {-# SCC bih_rayint_neg #-}
              nearest
               (if near < dr
                then (case fmin dr far of far' -> traverse r near far')
                else miss)
               (if dl < far
                then (case fmax dl near of near' -> traverse l near' far)
                else miss)
  in
     traverse root near (fmin d far)

-- Ray traversal with debug counter.  The counter gets incremented
-- when we hit a box.
rayint_debug_bih :: Bih tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> (Rayint tag mat, Int) 
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

packetint_bih :: Bih tag mat -> Ray -> Ray -> Ray -> Ray -> Flt -> [Texture tag mat] -> [tag] -> PacketResult tag mat
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

shadow_bih :: Bih m t -> Ray -> Flt -> Bool
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

inside_bih :: Bih t m -> Vec -> Bool
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
bound_bih :: Bih t m -> Bbox
bound_bih (Bih bb root) = bb

primcount_bih :: Bih t m -> Pcount
primcount_bih (Bih bb root) = pcadd (bihcount root) pcsinglebound
 where bihcount (BihLeaf s) = primcount s
       bihcount (BihBranch _ _ _ l r) = 
        pcadd (pcadd (bihcount l) (bihcount r)) pcsinglebound

instance Solid (Bih t m) t m where
 rayint = rayint_bih
 rayint_debug = rayint_debug_bih
 packetint = packetint_bih
 shadow = shadow_bih
 inside = inside_bih
 bound = bound_bih
 primcount = primcount_bih
 get_metainfo = get_metainfo_bih
