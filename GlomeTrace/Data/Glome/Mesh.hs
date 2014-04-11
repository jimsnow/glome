{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Glome.Mesh (mesh, tri) where
import Data.Glome.Vec
import Data.Glome.Solid
import qualified Data.Glome.Triangle as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import qualified Debug.Trace as DBG
import Control.DeepSeq(NFData, rnf)
import Control.Monad.Par
import Data.Int

-- Specialized tree type for triangle meshes.
-- The mesh holds a vector each for the triangle verticies,
-- the normals, the textures, and the tags, and each triangle is
-- just a collection of indicies into those vectors.
--
-- The tree itself is a Bounding Volume Hierarchy with a pair of
-- bounding boxes at each branch.  This seems to perform about as well
-- as BIH.  (The bounding boxes can be a tighter fit with fewer levels,
-- but testing rays against a bounding box is more expensive than just a
-- split plane.

-- | A Tri consists of three faces, three normal indicies, a texture index, and a tag index.
-- normals, tags, and textures are all optional.  (To disable, just use -1.)
data Tri = Tri !Int !Int !Int !Int !Int !Int !Int !Int deriving (Eq, Ord, Show)

tri = Tri

instance NFData Tri where
  rnf (Tri a b c n1 n2 n3 tex tag) = rnf a `seq` rnf b `seq` rnf c `seq` rnf n1 `seq` rnf n2 `seq` rnf n3 `seq` rnf tex `seq` rnf tag

data BVH = Leaf (UV.Vector Int) | Branch Bbox Bbox BVH BVH deriving Show

instance NFData BVH where
  rnf (Leaf ts) = rnf ts
  rnf (Branch lbb rbb l r) = rnf r `seq` rnf l 

data Mesh t m = Mesh (V.Vector Vec) (V.Vector Vec) (V.Vector Tri) (V.Vector (Texture t m)) (V.Vector t) Bbox BVH

instance Show (Mesh t m) where
  show (Mesh verts norms tris texs tags bb bvh) =
    "Mesh " ++ (show verts) ++ " " ++ (show tris) ++ " " ++ (show bb) ++ " " ++ (show bvh)

bbmid (Bbox p1 p2) = (vscale (vadd p1 p2) 0.5)

mesh :: V.Vector Vec -> V.Vector Vec -> V.Vector Tri -> (V.Vector (Texture t m)) -> V.Vector t-> Mesh t m
mesh verts norms tris texs tags =
  DBG.trace "got here" $ Mesh verts norms tris texs tags bbox (build_tree (UV.fromList [0..(tricount-1)]) bbox)
  where
    tricount = V.length tris
    bbox = bbpts $ V.toList verts

    branch lbb rbb l r = 
      if UV.length l > 1000 && UV.length r > 1000
      then
       runPar $ do
         ljob <- spawnP (build_tree l lbb)
         rjob <- spawnP (build_tree r rbb)
         l' <- get ljob
         r' <- get rjob
         return $ Branch lbb rbb l' r'
      else
        Branch lbb rbb (build_tree l lbb) (build_tree r rbb)

    build_tree :: UV.Vector Int -> Bbox -> BVH
    build_tree tris bb =
      let n = UV.length tris
      in if n < 3
         then Leaf tris
         else
           let mid = bbmid bb
               Vec xmid ymid zmid = mid
               sa = bbsa bb

               -- We'll try splitting along all three axes, and sorting big
               -- objects from small objects, then take the best result.
               -- This is slower than just picking the longest axis, but
               -- the resulting tree is more optimal.

               (lx,rx) = UV.partition (\t -> x (bbmid (tbbget t)) < xmid) tris
               (ly,ry) = UV.partition (\t -> y (bbmid (tbbget t)) < ymid) tris
               (lz,rz) = UV.partition (\t -> z (bbmid (tbbget t)) < zmid) tris
               (big,small) = UV.partition (\t -> bbsa (tbbget t) > sa * 0.4) tris

               lbbx = trisbb lx
               rbbx = trisbb rx
               lbby = trisbb ly
               rbby = trisbb ry
               lbbz = trisbb lz
               rbbz = trisbb rz
               lbbbig = trisbb big
               rbbsmall = trisbb small

               xcost = (cost lbbx lx + cost rbbx rx) * 1.1
               ycost = (cost lbby ly + cost rbby ry) * 1.1
               zcost = (cost lbbz lz + cost rbbz rz) * 1.1
               bcost = (cost lbbbig big + cost rbbsmall small) * 1.1
               lcost = cost bb tris
           in
             if lcost < xcost && lcost < ycost && lcost < zcost && lcost < bcost
             then Leaf tris
             else
               if xcost < ycost && xcost < zcost && xcost < bcost
               then branch lbbx rbbx lx rx
               else if ycost < zcost && ycost < bcost
                    then branch lbby rbby ly ry
                    else if zcost < bcost
                         then branch lbbz rbbz lz rz
                         else branch lbbbig rbbsmall big small 

    -- The cost of a bounding box is it's surface area times the number of
    -- elements inside.
    cost bb elems = bbsa bb * (fromIntegral $ UV.length elems)

    alltribbs :: V.Vector Bbox
    alltribbs =
      V.map (\(Tri a b c _ _ _ _ _) -> bbpts [vget a, vget b, vget c]) tris

    -- Get the Bbox for a vector of Tris
    trisbb :: UV.Vector Int -> Bbox
    trisbb tidxs = V.foldl' bbjoin empty_bbox (tribbs tidxs)

    -- Get the Bboxes for a vector of Tris
    tribbs :: UV.Vector Int -> V.Vector Bbox
    tribbs tris = V.map tbbget (V.convert tris)

    vget idx = verts V.! idx
    tbbget idx = alltribbs V.! idx
    texget idx = texs V.! idx
    tagget idx = tags V.! idx

rayint_mesh :: Mesh tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_mesh (Mesh verts norms tris texv tagv bb bvh) ray@(Ray o dir) depth texs tags =
  let ray_rcp = Ray o (vrcp dir)
      (# near, far #) = bbclip_ub_rcp ray_rcp bb
  in if near > far || near > depth || far < 0
     then raymiss
     else
       let rayint_tri i far =
             let (Tri ai bi ci ani bni cni texi tagi) = tris V.! i
                 a = verts V.! ai
                 b = verts V.! bi
                 c = verts V.! ci
                 tex = if texi == (-1) 
                       then texs
                       else (texv V.! texi) : texs
                 tag = if tagi == (-1)
                       then tags
                       else (tagv V.! tagi) : tags
             in
               if ani == (-1)
               then T.rayint_triangle (T.Triangle a b c) ray far tex tag
               else
                 let an = norms V.! ani
                     bn = norms V.! bni
                     cn = norms V.! cni
                 in T.rayint_trianglenorm (T.TriangleNorm a b c an bn cn) ray far tex tag

           traverse (Leaf tris) near far = V.foldl' nearest raymiss (V.map (\i -> rayint_tri i far) (V.convert tris)) 
           traverse (Branch lbb rbb l r) near far = 
              let (# lnear', lfar' #) = bbclip_ub_rcp ray_rcp lbb
                  (# rnear', rfar' #) = bbclip_ub_rcp ray_rcp rbb
                  lnear = max near lnear'
                  lfar  = min far lfar'
                  rnear = max near rnear'
                  rfar  = min far rfar'
              in
                if lnear < rnear
                then
                  let lresult = 
                        (if lnear > lfar || lnear > depth || lfar < 0
                         then raymiss
                         else traverse l lnear lfar)
                      rfar' = min rfar (ridepth lresult)
                  in
                    nearest
                      lresult
                      (if rnear > rfar' || rnear > depth || rfar' < 0
                       then raymiss
                       else traverse r rnear rfar)
                else
                  let rresult = 
                        (if rnear > rfar || rnear > depth || rfar < 0
                         then raymiss
                         else traverse r rnear rfar)
                      lfar' = min lfar (ridepth rresult)
                  in
                    nearest
                      rresult
                      (if lnear > lfar' || lnear > depth || lfar' < 0
                       then raymiss
                       else traverse l lnear lfar)       
  
       in traverse bvh near far
 

primcount_mesh (Mesh _ _ _ _ _ _ bvh) =
  pcount bvh
  where
    pcount (Leaf xs) = Pcount (UV.length xs, 0, 0)
    pcount (Branch _ _ l r) = pcadd (pcadd (pcount l) (pcount r)) (Pcount (0, 0, 1))

          
instance Solid (Mesh t m) t m where
 rayint = rayint_mesh
 shadow s r d = False
 inside _ _ = False
 bound (Mesh _ _ _ _ _ bb _) = bb
 primcount = primcount_mesh 
 --get_metainfo = 
