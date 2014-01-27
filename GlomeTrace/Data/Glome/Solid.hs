{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Glome.Solid where
import Data.Glome.Vec
import Data.Glome.Clr
import Data.List hiding (group)

-- | Ray intersection type.  If we hit, we store the distance from the ray
-- origin, the position, the normal, and the texture attached to the object.
-- We could just as easily have created a hit type and wrapped it in a Maybe.

data Rayint = RayHit {
 depth    :: !Flt,
 pos      :: !Vec,
 norm     :: !Vec,
 texture  :: Texture
} | RayMiss deriving Show

raymiss :: Rayint
raymiss = RayMiss

-- | Pick the closest of two Rayints
nearest :: Rayint -> Rayint -> Rayint
nearest a RayMiss = {-# SCC nearest_miss_a #-} a
nearest RayMiss b = {-# SCC nearest_miss_b #-} b
nearest a@(RayHit !da _ _ _) b@(RayHit !db _ _ _) =
 {-# SCC nearest_cmp #-}
 if da < db
 then a
 else b

-- | Pick the furthest of two Rayints
furthest :: Rayint -> Rayint -> Rayint
furthest _ RayMiss = RayMiss
furthest RayMiss _ = RayMiss
furthest a@(RayHit !da _ _ _) b@(RayHit !db _ _ _) =
 if da > db
 then a
 else b

-- | Test if a Rayint is a hit or a miss
hit :: Rayint -> Bool
hit (RayHit _ _ _ _) = True
hit RayMiss = False

-- | Extract a distance from a Rayint, with infinity for a miss
dist :: Rayint -> Flt
dist RayMiss = infinity
dist (RayHit d _ _ _) = d

--Packet Types--

-- | Sometimes, it's more efficient to trace multiple rays against an 
-- acceleration structure at the same time, provided the rays are almost
-- identical.  A PacketResult is the result of tracing 4 rays at once.

data PacketResult = PacketResult !Rayint !Rayint !Rayint !Rayint
packetmiss = PacketResult RayMiss RayMiss RayMiss RayMiss


nearest_packetresult :: PacketResult -> PacketResult -> PacketResult
nearest_packetresult !(PacketResult a1 a2 a3 a4) !(PacketResult b1 b2 b3 b4) =
 PacketResult (nearest a1 b1)
              (nearest a2 b2)
              (nearest a3 b3)
              (nearest a4 b4)

-- | Move a ray forward and test the new ray against an object.
-- Fix the depth of the result.  Useful in CSG
rayint_advance :: SolidItem -> Ray -> Flt -> Texture -> Flt -> Rayint
rayint_advance s r d t adv =
 let a = adv+delta
 in
  case (rayint s (ray_move r a) (d-a) t) of
   RayMiss -> RayMiss
   RayHit depth pos norm tex -> RayHit (depth+a) pos norm tex


--MATERIALS--

-- | Surface properties at a point on an object's surface.  We have color, 
-- reflection amount, refraction amount index of refraction, kd, ks, and shine.
-- These are parameters to a Whitted - style illumination model.

data Material = Material {clr :: !Color, 
                          refl, refr, ior, 
                          kd, ks, shine :: !Flt} deriving Show

-- | A texture is a function that takes a Rayint and returns a Material.
-- In other words, textures can vary based on location, normal, etc...
-- in arbitrary ways.
type Texture = Rayint -> Material

-- | This is sort of a no-op; textures are functions, and we don't have a
-- good way to show an arbitrary function
showTexture :: Texture -> String
showTexture t = show $ t RayMiss

instance Show Texture where
 show = showTexture

-- | Uniform white material
m_white = (Material c_white 0 0 0 1 0 2)
t_white ri = m_white

-- | Uniform texture
t_uniform :: Material -> Texture
t_uniform m = \x -> m

interp :: Flt -> Flt -> Flt -> Flt
interp scale a b =
 scale*a + (1-scale)*b

-- | Interpolate between textures.  
-- Not really correct, but we'll go with it for now.
m_interp :: Material -> Material -> Flt -> Material
m_interp m1 m2 scale =
 let (Material m1c m1refl m1refr m1ior m1kd m1ks m1shine) = m1
     (Material m2c m2refl m2refr m2ior m2kd m2ks m2shine) = m2
     intp  = interp scale
     c     = cadd (cscale m1c scale) (cscale m2c (1-scale))
     refl  = intp m1refl m2refl
     refr  = intp m1refr m2refr
     ior   = intp m1ior m2ior
     kd    = intp m1kd m2kd
     ks    = intp m1ks m2ks
     shine = intp m1shine m2shine
 in (Material c refl refr ior kd ks shine)

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
debug_wrap :: (Rayint,Int) -> Int -> (Rayint,Int)
debug_wrap (ri,a) b = (ri,(a+b))

nearest_debug :: (Rayint,Int) -> (Rayint,Int) -> (Rayint,Int)
nearest_debug (ari, an) (bri, bn) = ((nearest ari bri),(an+bn))

--SOLID CLASS--

-- | A solid is something we can test a ray against or do inside/outside tests.
-- Some of these are simple solids like Sphere or Triangle, but others
-- are composite solids than have other solids as children.

class (Show a) => Solid a where

 -- | Test a ray against a solid, returning a ray intersection.
 -- The distance parameter is used to specify a max distance.
 -- If it's further away, we aren't interested in the intersection.
 -- The texture parameter is a default texture we use, if it's not
 -- overridden by a more specific texture.
 rayint :: a  -- ^ object to test against
        -> Ray -- ^ ray
        -> Flt -- ^ maximum distance we care about
        -> Texture -- ^ default texture
        -> Rayint  -- ^ we return a Rayint describing the hit location

 -- | Same as rayint, but return a count of the number of
 -- primitives checked.  Useful for optimizing acceleration structures.
 rayint_debug :: a -> Ray -> Flt -> Texture -> (Rayint, Int)

 -- | Trace four rays at once against a solid.
 packetint :: a -> Ray -> Ray -> Ray -> Ray -> Flt -> Texture -> PacketResult 

 -- | Shadow test - we just return a Bool rather than return a 
 -- a full Rayint.
 shadow :: a -> Ray -> Flt -> Bool

 -- | Test if a point is inside an object.  Useful for CSG.
 -- Objects with no volume just return False.
 inside :: a -> Vec -> Bool

 -- | Generate an axis-aligned bounding box than completely encloses
 -- the object.  For performance, it is important that this fits as 
 -- tight as possible.
 bound  :: a -> Bbox

 -- | Most simple objects just return themselves as a singleton list,
 -- but for composite objects, we flatten the structure out and 
 -- return a list.  We usually do this prior to re-building a 
 -- composite object in a (hopefully) more efficient fashion.
 tolist :: a -> [SolidItem]

 -- | Create a new object transformed by some transformation.  The
 -- reason this method exists is so we can override it for the
 -- Instance type - if we transform a transformation, we should
 -- combine the two matricies into one.
 -- Most objects can use the default implementation.
 transform :: a -> [Xfm] -> SolidItem

 -- | Used by flatten_transform.  I don't really remember how it works. 
 transform_leaf :: a -> [Xfm] -> SolidItem

 -- | Take a composite object inside a transform, and turn it into
 -- a group of individually-transformed objects.  Most objects 
 -- can use the defaut implementation.
 flatten_transform :: a -> [SolidItem]

 -- | Count the number of primitives, transforms, and bounding
 -- objects in a scene.  Simple objects can just use the default,
 -- which is to return a single primitive.
 primcount :: a -> Pcount

 -- | This is for counting bih split planes and the like, for
 -- performance tuning and debugging.  Most objects can use
 -- the default implementation.
 rayint_debug s !r !d t = ((rayint s r d t),0)

 -- | Sometimes, we can improve performance by 
 -- intersecting 4 rays at once.  This is 
 -- especially true of acceleration structures.
 -- The default implementation is to fall back on mono-rays.
 packetint s !r1 !r2 !r3 !r4 !d t = 
  PacketResult (rayint s r1 d t)
               (rayint s r2 d t)
               (rayint s r3 d t)
               (rayint s r4 d t)

 -- if there is no shadow function, we fall back on rayint
 shadow s !r !d =
  case (rayint s r d t_white) of
   RayHit _ _ _ _ -> True
   RayMiss -> False

 -- There's a name for what a bunch of these functions
 -- are trying to do (but poorly): what I really want is
 -- a "catamorphism".

 -- This is here so we can flatten a group of groups
 -- into a single group; the default is fine for everything
 -- but groups and Void and SolidItem.
 tolist a = [SolidItem (a)]
 
 -- Method to transform an object; the default works fine
 -- except for instances themselves, which will want to
 -- collapse the two transformations into a sigle transform.
 transform a xfm = SolidItem $ Instance (SolidItem a) (compose xfm)

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


-- | We create an existential type for solids so we can emded them
-- in composite types without know what kind of solid it is.
-- http://notes-on-haskell.blogspot.com/2007/01/proxies-and-delegation-vs-existential.html

data SolidItem = forall a. Solid a => SolidItem a

instance Solid SolidItem where
 rayint (SolidItem s) !r !d t = rayint s r d t
 packetint (SolidItem s) !r1 !r2 !r3 !r4 !d t = packetint s r1 r2 r3 r4 d t
 rayint_debug (SolidItem s) r d t = rayint_debug s r d t
 shadow (SolidItem s) !r !d = shadow s r d
 inside (SolidItem s) pt = inside s pt
 bound  (SolidItem s) = bound s
 tolist (SolidItem s) = tolist s -- don't wrap in a redundant SolidItem like everything else
 transform (SolidItem s) xfm = transform s xfm -- same here
 transform_leaf (SolidItem s) xfm = transform_leaf s xfm -- and here
 flatten_transform (SolidItem s) = [SolidItem (flatten_transform s)] -- and here
 primcount (SolidItem s) = primcount s

instance Show SolidItem where
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

group :: [SolidItem] -> SolidItem
group [] = SolidItem Void
group (sld:[]) = sld
group slds = SolidItem (flatten_group slds)

-- | Smash a group of groups into a single group,
-- so we can build an efficient bounding heirarchy

flatten_group :: [SolidItem] -> [SolidItem]
flatten_group slds = concat (map tolist slds)

-- this lets us treat lists of SolidItems as regular Solids
rayint_group :: [SolidItem] -> Ray -> Flt -> Texture -> Rayint
rayint_group [] _ _ _ = RayMiss
rayint_group (x:xs) !r !d t = nearest (rayint x r d t) (rayint_group xs r d t)

{-- this is not measurably faster
rayint_group slds r d t = go slds RayMiss
 where go [] res = res
       go (x:xs) res = go xs $ nearest (rayint x r d t) res
--}

packetint_group :: [SolidItem] -> Ray -> Ray -> Ray -> Ray -> Flt -> Texture -> PacketResult
packetint_group [] !r1 !r2 !r3 !r4 !d t = packetmiss
packetint_group (x:xs) !r1 !r2 !r3 !r4 !d t = 
 nearest_packetresult (packetint x r1 r2 r3 r4 d t) 
                      (packetint_group xs r1 r2 r3 r4 d t)

rayint_debug_group :: [SolidItem] -> Ray -> Flt -> Texture -> (Rayint,Int)
rayint_debug_group [] _ _ _ = (RayMiss,0)
rayint_debug_group (x:xs) !r !d t = 
 nearest_debug (rayint_debug x r d t) 
               (rayint_debug_group xs r d t)

shadow_group :: [SolidItem] -> Ray -> Flt -> Bool
shadow_group [] !r !d = False
shadow_group (x:xs) r d = (shadow x r d) || (shadow_group xs r d)

inside_group :: [SolidItem] -> Vec -> Bool
inside_group slds pt =
 foldl' (||) False (map (\x -> inside x pt) slds)

bound_group :: [SolidItem] -> Bbox
bound_group slds = 
 foldl' bbjoin empty_bbox (map bound slds)

transform_leaf_group :: [SolidItem] -> [Xfm] -> SolidItem
transform_leaf_group slds xfms =
 SolidItem $ map (\x -> transform_leaf x xfms) (tolist slds)

primcount_group :: [SolidItem] -> Pcount
primcount_group slds = foldl (pcadd) (Pcount (0,0,0)) (map primcount slds)

instance Solid [SolidItem] where
 rayint = rayint_group
 packetint = packetint_group
 rayint_debug = rayint_debug_group
 shadow = shadow_group
 inside = inside_group
 bound = bound_group
 tolist a = concat $ map tolist a
 transform_leaf = transform_leaf_group
 flatten_transform a = concat $ map flatten_transform a
 primcount = primcount_group

-- VOID --

-- | A Void is a non-object, that we treat as if it were
-- one.  This is functionally equivalent to an empty Group.
-- (Originally I called this "Nothing", but that
-- conflicted with the prelude maybe type, so I call
-- it "Void" instead) 
data Void = Void deriving Show

nothing = SolidItem Void

instance Solid Void where
 rayint Void _ _ _ = RayMiss
 packetint Void _ _ _ _ _ _ = packetmiss
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

data Instance = Instance SolidItem Xfm deriving Show

rayint_instance :: Instance -> Ray -> Flt -> Texture -> Rayint
rayint_instance !(Instance sld xfm) !(Ray orig dir) !d t =
 let newdir  = invxfm_vec xfm dir
     neworig = invxfm_point xfm orig
     lenscale = vlen newdir
     invlenscale = 1/lenscale
 in
  case (rayint sld (Ray neworig (vscale newdir invlenscale)) (d*lenscale) t) of
   RayMiss -> RayMiss
   RayHit depth pos n tex -> RayHit (depth*invlenscale) 
                                    (xfm_point xfm pos) 
                                    (vnorm (invxfm_norm xfm n)) 
                                    tex

packetint_instance :: Instance -> Ray -> Ray -> Ray -> Ray -> Flt -> Texture -> PacketResult
packetint_instance !(Instance sld xfm) !(Ray orig1 dir1) !(Ray orig2 dir2) 
                                      !(Ray orig3 dir3) !(Ray orig4 dir4) d t =
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
                         (d*lenscale1) t
      PacketResult ri1 ri2 ri3 ri4 = pr 
      fix ri ils = 
       case ri of 
        RayMiss -> RayMiss
        RayHit depth pos n tex -> RayHit (depth*ils) 
                                         (xfm_point xfm pos) 
                                         (vnorm (invxfm_norm xfm n)) 
                                         tex
  in PacketResult (fix ri1 invlenscale1)
                  (fix ri2 invlenscale2)
                  (fix ri3 invlenscale3)
                  (fix ri4 invlenscale4)

-- ugh, code duplication
rayint_debug_instance :: Instance -> Ray -> Flt -> Texture -> (Rayint,Int)
rayint_debug_instance (Instance sld xfm) (Ray orig dir) d t =
 let newdir  = invxfm_vec xfm dir
     neworig = invxfm_point xfm orig
     lenscale = vlen newdir
     invlenscale = 1/lenscale
 in
  case (rayint_debug sld (Ray neworig (vscale newdir invlenscale)) (d*lenscale) t) of
   (RayMiss, count) -> (RayMiss, count)
   (RayHit depth pos n tex, count) -> (RayHit (depth*invlenscale) 
                                         (xfm_point xfm pos) 
                                         (vnorm (invxfm_norm xfm n)) 
                                         tex, count)

shadow_instance :: Instance -> Ray -> Flt -> Bool
shadow_instance !(Instance sld xfm) !(Ray orig dir) !d =
 let newdir  = invxfm_vec xfm dir
     neworig = invxfm_point xfm orig
     lenscale = vlen newdir
     invlenscale = 1/lenscale
 in
  shadow sld (Ray neworig (vscale newdir invlenscale)) (d*lenscale)

inside_instance :: Instance -> Vec -> Bool
inside_instance (Instance s xfm) pt =
 inside s (invxfm_point xfm pt)

bound_instance :: Instance -> Bbox
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

transform_instance :: Instance -> [Xfm] -> SolidItem
transform_instance (Instance s xfm2) xfm1 =
 transform s [compose ([xfm2]++xfm1) ]

transform_leaf_instance :: Instance -> [Xfm] -> SolidItem
transform_leaf_instance (Instance s xfm2) xfm1 =
 transform_leaf s [compose ([xfm2]++xfm1) ]

-- Flatten_transform attempts to push all transformations 
-- in a heirarchy out to the leaf nodes.  The case we're
-- interested in here is an instance of a group, and we 
-- want to replace that with a group of individually 
-- transformed instances.  This could be construed as a
-- waste of memory, but in some cases it's necessary.

flatten_transform_instance :: Instance -> [SolidItem]
flatten_transform_instance (Instance s xfm) = 
 [SolidItem $ transform_leaf s [xfm]]
 -- group $ map (\x -> transform (flatten_transform x) [xfm]) (tolist s)

primcount_instance :: Instance -> Pcount
primcount_instance (Instance s xfm) = pcadd (primcount s) pcsinglexfm

instance Solid Instance where
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
