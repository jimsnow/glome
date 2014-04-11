{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Glome.Solid where
import Data.Glome.Vec
import Data.Glome.Clr
import Data.List hiding (group)

-- | Ray intersection type.  If we hit, we store the distance from the ray
-- origin, the position, the normal, the transformed ray, UV coordinates
-- (plus a 3rd coordinate we'll call W) and the texture 
-- and tag stacks attached to the object.
data Rayint tag mat = RayHit {
 ridepth' :: !Flt,
 ripos    :: !Vec,
 rinorm   :: !Vec,
 riray    :: !Ray,
 riuvw    :: !Vec,
 ritex    :: [Texture tag mat],
 ritag    :: [tag]
} | RayMiss deriving Show

raymiss :: Rayint tag mat
raymiss = RayMiss

ridepth RayMiss = infinity
ridepth ri = ridepth' ri

-- | Pick the closest of two Rayints
nearest :: Rayint tag mat -> Rayint tag mat -> Rayint tag mat
nearest a RayMiss = {-# SCC nearest_miss_a #-} a
nearest RayMiss b = {-# SCC nearest_miss_b #-} b
nearest a@(RayHit !da _ _ _ _ _ _) b@(RayHit !db _ _ _ _ _ _) =
 {-# SCC nearest_cmp #-}
 if da < db
 then a
 else b

-- | Pick the furthest of two Rayints
furthest :: Rayint tag mat -> Rayint tag mat -> Rayint tag mat
furthest _ RayMiss = RayMiss
furthest RayMiss _ = RayMiss
furthest a@(RayHit !da _ _ _ _ _ _) b@(RayHit !db _ _ _ _ _ _) =
 if da > db
 then a
 else b

-- | Test if a Rayint is a hit or a miss
hit :: Rayint tag mat -> Bool
hit (RayHit _ _ _ _ _ _ _) = True
hit RayMiss = False

-- | Extract a distance from a Rayint, with infinity for a miss
dist :: Rayint tag mat -> Flt
dist RayMiss = infinity
dist (RayHit d _ _ _ _ _ _) = d

--Packet Types--

-- | Sometimes, it's more efficient to trace multiple rays against an 
-- acceleration structure at the same time, provided the rays are almost
-- identical.  A PacketResult is the result of tracing 4 rays at once.

data PacketResult tag mat = PacketResult (Rayint tag mat) (Rayint tag mat) (Rayint tag mat) (Rayint tag mat)

packetmiss = PacketResult RayMiss RayMiss RayMiss RayMiss


nearest_packetresult :: PacketResult tag mat -> PacketResult tag mat -> PacketResult tag mat
nearest_packetresult !(PacketResult a1 a2 a3 a4) !(PacketResult b1 b2 b3 b4) =
 PacketResult (nearest a1 b1)
              (nearest a2 b2)
              (nearest a3 b3)
              (nearest a4 b4)

-- | Move a ray forward and test the new ray against an object.
-- Fix the depth of the result.  Useful in CSG
rayint_advance :: SolidItem tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Flt -> Rayint tag mat
rayint_advance s r d t tags adv =
 let a = adv+delta
 in
  case (rayint s (ray_move r a) (d-a) t tags) of
   RayMiss -> RayMiss
   RayHit depth pos norm ray uvw tex tags -> RayHit (depth+a) pos norm ray uvw tex tags


-- | A texture is a function that takes a Rayint and returns a material.
-- A material will later be rendered by a shader (which in turn can
-- append more tags).
type Texture tag mat = Ray -> Rayint tag mat -> mat

-- | This is sort of a no-op; textures are functions, and we don't have a
-- good way to show an arbitrary function
instance Show (Texture t m) where
 show t = "Texture"

--utility functions for "primcount"
newtype Pcount = Pcount (Int,Int,Int) deriving Show

pcadd :: Pcount -> Pcount -> Pcount
pcadd (Pcount (a1,a2,a3)) (Pcount (b1,b2,b3)) = Pcount (a1+b1, a2+b2, a3+b3)

asbound :: Pcount -> Pcount
asbound (Pcount (a,b,c)) = Pcount (0,b,a+c)

pcsinglexfm ::  Pcount
pcsinglexfm = Pcount (0,1,0)

pcsingleprim :: Pcount
pcsingleprim = Pcount (1,0,0)

pcsinglebound :: Pcount
pcsinglebound = Pcount (0,0,1)

pcnone :: Pcount
pcnone = Pcount (0,0,0)

-- utility functions for rayint_debug
debug_wrap :: (Rayint tag mat, Int) -> Int -> (Rayint tag mat, Int)
debug_wrap (ri, a) b = (ri, (a+b))

nearest_debug :: (Rayint tag mat, Int) -> (Rayint tag mat, Int) -> (Rayint tag mat, Int)
nearest_debug (ari, an) (bri, bn) = ((nearest ari bri),(an+bn))

--SOLID CLASS--

-- | A solid is something we can test a ray against or do inside/outside tests.
-- Some of these are simple solids like Sphere or Triangle, but others
-- are composite solids than have other solids as children.

class (Show s) => Solid s t m | s -> t, s -> m where

 -- | Test a ray against a solid, returning a ray intersection.
 -- The distance parameter is used to specify a max distance.
 -- If it's further away, we aren't interested in the intersection.
 -- The "b" parameter is a default tag, if it's not
 -- overridden by a more specific tag (which is useful if we need to be
 -- able to identify the thing that was hit).
 rayint :: s             -- ^ object to test against
        -> Ray           -- ^ ray
        -> Flt           -- ^ maximum distance we care about
        -> [Texture t m] -- ^ current texture stack (Tex object pushes new textures)
        -> [t]           -- ^ tag stack (Tag object pushes new tags)
        -> Rayint t m    -- ^ we return a Rayint describing the hit location

 -- | Same as rayint, but return a count of the number of
 -- primitives checked.  Useful for optimizing acceleration structures.
 rayint_debug :: s -> Ray -> Flt -> [Texture t m] -> [t] -> (Rayint t m, Int)

 -- | Trace four rays at once against a solid.
 packetint :: s -> Ray -> Ray -> Ray -> Ray -> Flt -> [Texture t m] -> [t] -> PacketResult t m

 -- | Shadow test - we just return a Bool rather than return a 
 -- a full Rayint.
 shadow :: s -> Ray -> Flt -> Bool

 -- | Test if a point is inside an object.  Useful for CSG.
 -- Objects with no volume just return False.
 inside :: s -> Vec -> Bool

 -- | Generate an axis-aligned bounding box than completely encloses
 -- the object.  For performance, it is important that this fits as 
 -- tight as possible.
 bound  :: s -> Bbox

 -- | Most simple objects just return themselves as a singleton list,
 -- but for composite objects, we flatten the structure out and 
 -- return a list.  We usually do this prior to re-building a 
 -- composite object in a (hopefully) more efficient fashion.
 tolist :: s -> [SolidItem t m]

 -- | Create a new object transformed by some transformation.  The
 -- reason this method exists is so we can override it for the
 -- Instance type - if we transform a transformation, we should
 -- combine the two matricies into one.
 -- Most objects can use the default implementation.
 transform :: s -> [Xfm] -> SolidItem t m

 -- | Used by flatten_transform.  I don't really remember how it works. 
 transform_leaf :: s -> [Xfm] -> SolidItem t m

 -- | Take a composite object inside a transform, and turn it into
 -- a group of individually-transformed objects.  Most objects 
 -- can use the defaut implementation.
 flatten_transform :: s -> [SolidItem t m]

 -- | Count the number of primitives, transforms, and bounding
 -- objects in a scene.  Simple objects can just use the default,
 -- which is to return a single primitive.
 primcount :: s -> Pcount

 -- | Get texture and tag data for a primitive, from a point.
 get_metainfo :: s -> Vec -> ([Texture t m], [t])

 -- | This is for counting bih split planes and the like, for
 -- performance tuning and debugging.  Most objects can use
 -- the default implementation.
 rayint_debug s !r !d t tags = ((rayint s r d t tags), 0)

 -- | Sometimes, we can improve performance by 
 -- intersecting 4 rays at once.  This is 
 -- especially true of acceleration structures.
 -- The default implementation is to fall back on mono-rays.
 packetint s !r1 !r2 !r3 !r4 !d t tags = 
  PacketResult (rayint s r1 d t tags)
               (rayint s r2 d t tags)
               (rayint s r3 d t tags)
               (rayint s r4 d t tags)

 -- if there is no shadow function, we fall back on rayint
 shadow s !r !d =
  case (rayint s r d undefined []) of
   RayHit _ _ _ _ _ _ _ -> True
   RayMiss -> False

 -- There's a name for what a bunch of these functions
 -- are trying to do (but poorly): what I really want is
 -- a "catamorphism".

 -- This is here so we can flatten a group of groups
 -- into a single group; the default is fine for everything
 -- but groups and Void and SolidItem.
 tolist s = [SolidItem s]
 
 -- Method to transform an object; the default works fine
 -- except for instances themselves, which will want to
 -- collapse the two transformations into a sigle transform.
 transform s xfm = SolidItem $ Instance (SolidItem s) (compose xfm)

 -- This is used by flatten_transform below.  For simple objects, it 
 -- works the same as transform, but for groups it transforms all the
 -- objects individually.
 transform_leaf = transform

 -- This prepares a composite primitive to be fed into the bih constructor
 -- by pushing all the transformations out to the leaves and 
 -- throwing away manual bounding structures.  For simple primitives, this
 -- is a no-op.
 flatten_transform = tolist

 -- Figure out how complicated the scene really is.
 -- Returns (primitives, matricies, bounding objects/planes).
 -- Also, it forces the full construction of acceleration structures.
 primcount s = pcsingleprim

 -- Lookup texture and tag info at a point.
 get_metainfo s v = ([],[])


-- | We create an existential type for solids so we can emded them
-- in composite types without know what kind of solid it is.
-- http://notes-on-haskell.blogspot.com/2007/01/proxies-and-delegation-vs-existential.html

data SolidItem t m = forall s. Solid s t m => SolidItem s

instance Solid (SolidItem t m) t m where
 rayint (SolidItem s) !r !d t tags = rayint s r d t tags
 packetint (SolidItem s) !r1 !r2 !r3 !r4 !d t tags = packetint s r1 r2 r3 r4 d t tags
 rayint_debug (SolidItem s) r d t tags = rayint_debug s r d t tags
 shadow (SolidItem s) !r !d = shadow s r d
 inside (SolidItem s) pt = inside s pt
 bound  (SolidItem s) = bound s
 tolist (SolidItem s) = tolist s -- don't wrap in a redundant SolidItem like everything else
 transform (SolidItem s) xfm = transform s xfm -- same here
 transform_leaf (SolidItem s) xfm = transform_leaf s xfm -- and here
 flatten_transform (SolidItem s) = [SolidItem (flatten_transform s)] -- and here
 primcount (SolidItem s) = primcount s
 get_metainfo (SolidItem s) v = get_metainfo s v

instance Show (SolidItem t m) where
 show (SolidItem s) = "SI " ++ show s

-- we implement "group", "void", and "instance" here because they're
-- used by some of the other primitives

-- GROUP --
--
-- | A group is just a list of objects.  Sometimes its convenient to be 
-- able to treat a group as if it were a single object, and that is 
-- exactly what we do here.  The ray intersection routine tests the ray 
-- against each object in turn.  Not very efficient
-- for large groups, but this is a useful building block for
-- constructing the leaves of acceleration structures.  (See the bih
-- module.)

group :: [SolidItem t m] -> SolidItem t m
group [] = SolidItem Void
group (sld:[]) = sld
group slds = SolidItem (flatten_group slds)

-- | Smash a group of groups into a single group,
-- so we can build an efficient bounding heirarchy

flatten_group :: Solid s t m => [s] -> [SolidItem t m]
flatten_group slds = concat (map tolist slds)

paircat :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
paircat (a1,b1) (a2,b2) = (a1++a2, b1++b2)

{-
-- Heterogeneous lists of primitives
instance Solid [SolidItem t m] t m where
 rayint xs r d t tags              = foldl' nearest RayMiss (map (\s -> rayint s r d t tags) xs)
 packetint xs r1 r2 r3 r4 d t tags = foldl' nearest_packetresult packetmiss (map (\s -> packetint s r1 r2 r3 r4 d t tags) xs)
 rayint_debug xs r d t tags        = foldl' nearest_debug (RayMiss,0) (map (\s -> rayint_debug s r d t tags) xs)
 shadow xs r d                     = foldl' (||) False (map (\s -> shadow s r d) xs)
 inside xs pt                      = foldl' (||) False (map (\x -> inside x pt) xs)
 bound xs                          = foldl' bbjoin empty_bbox (map bound xs)
 tolist a                          = concat $ map tolist a
 transform_leaf xs xfms            = SolidItem $ map (\x -> transform_leaf x xfms) (tolist xs)
 flatten_transform a               = concat $ map flatten_transform a
 primcount xs                      = foldl (pcadd) (Pcount (0,0,0)) (map primcount xs)
 get_metainfo xs v                 = foldl (\acc x -> if inside x v
                                                         then paircat (get_metainfo x v) acc
                                                         else acc) ([],[]) xs
-}

-- Homogeneous lists of primitives
instance Solid s t m => Solid [s] t m where
 rayint xs r d t tags              = foldl' nearest RayMiss (map (\s -> rayint s r d t tags) xs)
 packetint xs r1 r2 r3 r4 d t tags = foldl' nearest_packetresult packetmiss (map (\s -> packetint s r1 r2 r3 r4 d t tags) xs)
 rayint_debug xs r d t tags        = foldl' nearest_debug (RayMiss,0) (map (\s -> rayint_debug s r d t tags) xs)
 shadow xs r d                     = foldl' (||) False (map (\s -> shadow s r d) xs)
 inside xs pt                      = foldl' (||) False (map (\x -> inside x pt) xs)
 bound xs                          = foldl' bbjoin empty_bbox (map bound xs)
 tolist a                          = concat $ map tolist a
 transform_leaf xs xfms            = SolidItem $ map (\x -> transform_leaf x xfms) (tolist xs)
 flatten_transform a               = concat $ map flatten_transform a
 primcount xs                      = foldl (pcadd) (Pcount (0,0,0)) (map primcount xs)
 get_metainfo xs v                 = foldl (\acc x -> if inside x v
                                                         then paircat (get_metainfo x v) acc
                                                         else acc) ([],[]) xs


-- VOID --

-- | A Void is a non-object, that we treat as if it were
-- one.  This is functionally equivalent to an empty Group.
-- (Originally I called this "Nothing", but that
-- conflicted with the prelude maybe type, so I call
-- it "Void" instead) 
data Void t m = Void deriving Show

nothing = SolidItem Void

instance Solid (Void t m) t m where
 rayint Void _ _ _ _ = RayMiss
 packetint Void _ _ _ _ _ _ _ = packetmiss
 shadow Void _ _ = False
 inside Void _ = False
 bound  Void = empty_bbox
 tolist Void = []
 transform Void xfms = SolidItem Void 

-- INSTANCE --
--
-- | An instance is a primitive that has been modified
-- by a transformation (i.e. some combination of
-- translation, rotation, and scaling).  This is a
-- reasonably space-efficient way of making multiple copies
-- of a complex object.
--
-- Usually, the application doesn't need to create an 
-- instance directly, but should use "transform" on an
-- existing object.
-- 
-- It's unfortunate that "instance" is also a reserved word.  
-- "instance Solid Instance where..." is a little confusing.
-- 
-- This would be better in its own module, but we need
-- "Instance" to be defined here so we can define the default 
-- implementation of "transform" in terms on Instance.
-- (Mutually recursive modules would be useful, if I could
-- get them to work.)
--
-- Another good reason to include Instance in Solid.hs
-- is that it's referenced from Cone.hs

data Instance t m = Instance (SolidItem t m) Xfm deriving Show

rayint_instance :: Instance tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_instance !(Instance sld xfm) !(Ray orig dir) !d t tags =
 let newdir  = invxfm_vec xfm dir
     neworig = invxfm_point xfm orig
     lenscale = vlen newdir
     invlenscale = 1/lenscale
 in
  case (rayint sld (Ray neworig (vscale newdir invlenscale)) (d*lenscale) t tags) of
   RayMiss -> RayMiss
   RayHit depth pos n ray uvw tex tags -> RayHit (depth*invlenscale) 
                                                 (xfm_point xfm pos) 
                                                 (vnorm (invxfm_norm xfm n)) 
                                                 ray
                                                 uvw
                                                 tex
                                                 tags

packetint_instance :: Instance tag mat -> Ray -> Ray -> Ray -> Ray -> Flt -> [Texture tag mat] -> [tag] -> PacketResult tag mat
packetint_instance !(Instance sld xfm) !(Ray orig1 dir1) !(Ray orig2 dir2) 
                                       !(Ray orig3 dir3) !(Ray orig4 dir4) d t tags =
 let newdir1  = invxfm_vec xfm dir1
     newdir2  = invxfm_vec xfm dir2
     newdir3  = invxfm_vec xfm dir3
     newdir4  = invxfm_vec xfm dir4
     neworig1 = invxfm_point xfm orig1
     neworig2 = invxfm_point xfm orig2
     neworig3 = invxfm_point xfm orig3
     neworig4 = invxfm_point xfm orig4
     lenscale1 = vlen newdir1
     lenscale2 = vlen newdir2
     lenscale3 = vlen newdir3
     lenscale4 = vlen newdir4
     invlenscale1 = 1/lenscale1
     invlenscale2 = 1/lenscale2
     invlenscale3 = 1/lenscale3
     invlenscale4 = 1/lenscale4
 in
  let pr = packetint sld (Ray neworig1 (vscale newdir1 invlenscale1)) 
                         (Ray neworig2 (vscale newdir2 invlenscale2)) 
                         (Ray neworig3 (vscale newdir3 invlenscale3)) 
                         (Ray neworig4 (vscale newdir4 invlenscale4)) 
                         (d*lenscale1) t tags
      PacketResult ri1 ri2 ri3 ri4 = pr 
      fix ri ils = 
       case ri of 
        RayMiss -> RayMiss
        RayHit depth pos n ray uvw tex tags -> RayHit (depth*ils) 
                                                      (xfm_point xfm pos) 
                                                      (vnorm (invxfm_norm xfm n))
                                                      ray
                                                      uvw
                                                      tex
                                                      tags
  in PacketResult (fix ri1 invlenscale1)
                  (fix ri2 invlenscale2)
                  (fix ri3 invlenscale3)
                  (fix ri4 invlenscale4)

-- ugh, code duplication
rayint_debug_instance :: Instance tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag]-> (Rayint tag mat, Int)
rayint_debug_instance (Instance sld xfm) (Ray orig dir) d t tags =
 let newdir  = invxfm_vec xfm dir
     neworig = invxfm_point xfm orig
     lenscale = vlen newdir
     invlenscale = 1/lenscale
 in
  case rayint_debug sld (Ray neworig (vscale newdir invlenscale)) (d*lenscale) t tags of
   (RayMiss, count) -> (RayMiss, count)
   (RayHit depth pos n ray uvw tex tags, count) -> (RayHit (depth*invlenscale) 
                                                           (xfm_point xfm pos) 
                                                           (vnorm (invxfm_norm xfm n))
                                                           ray
                                                           uvw
                                                           tex
                                                           tags, count)

shadow_instance :: Instance tag mat -> Ray -> Flt -> Bool
shadow_instance !(Instance sld xfm) !(Ray orig dir) !d =
 let newdir  = invxfm_vec xfm dir
     neworig = invxfm_point xfm orig
     lenscale = vlen newdir
     invlenscale = 1/lenscale
 in
  shadow sld (Ray neworig (vscale newdir invlenscale)) (d*lenscale)

inside_instance :: Instance tag mat -> Vec -> Bool
inside_instance (Instance s xfm) pt =
 inside s (invxfm_point xfm pt)

bound_instance :: Instance tag mat -> Bbox
bound_instance (Instance sld xfm) =
 let (Bbox (Vec p1x p1y p1z) (Vec p2x p2y p2z)) = bound sld
     pxfm = xfm_point xfm
 in
     bbpts  [(pxfm (Vec x y z)) | x <- [p1x,p2x],
                                  y <- [p1y,p2y],
                                  z <- [p1z,p2z]]

-- If we try to create a transformation of
-- a transformation, we can merge those
-- into a single transformation.

-- This ought to be tested to verify this
-- is really applying transforms in the
-- correct order...

transform_instance :: Instance tag mat -> [Xfm] -> SolidItem tag mat
transform_instance (Instance s xfm2) xfm1 =
 transform s [compose ([xfm2]++xfm1) ]

transform_leaf_instance :: Instance tag mat -> [Xfm] -> SolidItem tag mat
transform_leaf_instance (Instance s xfm2) xfm1 =
 transform_leaf s [compose ([xfm2]++xfm1) ]

-- Flatten_transform attempts to push all transformations 
-- in a heirarchy out to the leaf nodes.  The case we're
-- interested in here is an instance of a group, and we 
-- want to replace that with a group of individually 
-- transformed instances.  This could be construed as a
-- waste of memory, but in some cases it's necessary.

flatten_transform_instance :: Instance tag mat -> [SolidItem tag mat]
flatten_transform_instance (Instance s xfm) = 
 [SolidItem $ transform_leaf s [xfm]]
 -- group $ map (\x -> transform (flatten_transform x) [xfm]) (tolist s)

primcount_instance :: Instance tag mat -> Pcount
primcount_instance (Instance s xfm) = pcadd (primcount s) pcsinglexfm

get_metainfo_instance :: Instance tag mat -> Vec -> ([Texture tag mat], [tag])
get_metainfo_instance (Instance s xfm) v =
  get_metainfo s (invxfm_point xfm v)

instance Solid (Instance t m) t m where
 rayint = rayint_instance
 packetint = packetint_instance
 rayint_debug = rayint_debug_instance
 shadow = shadow_instance
 inside = inside_instance
 bound  = bound_instance
 transform = transform_instance
 transform_leaf = transform_leaf_instance
 flatten_transform = flatten_transform_instance
 primcount = primcount_instance
 get_metainfo = get_metainfo_instance
