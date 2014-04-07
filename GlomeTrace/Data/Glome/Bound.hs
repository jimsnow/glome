{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Glome.Bound (bound_object) where
import Data.Glome.Vec
import Data.Glome.Solid

-- Bounding objects: we can use any object as a bounding
-- object for any other object; if a ray misses the
-- bounding object, we can assume it missed the bounded
-- object as well.  Unlike bih, setting up bounds is a manual
-- process.  It is important that the bounded object is
-- completely inside the bounding object.

-- The bounding object should have a cheaper intersection test than
-- the bounded object for this to be useful.

-- The first SolidItem is the bounding object, the second
-- is the bounded object.
data Bound t m = Bound (SolidItem t m) (SolidItem t m) deriving Show

-- | Use the first object as a bounding volume for the second
-- object.  If a ray misses the first object, it is assumed to
-- miss the second object.  Used primarily to improve performance.
-- In general, bih will usually perform better than 
-- manually-constructed bounds, though.

bound_object :: SolidItem t m -> SolidItem t m -> SolidItem t m
bound_object a b = SolidItem $ Bound a b

rayint_bound :: Bound tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_bound (Bound sa sb) r d t tags =
 let (Ray orig _) = r
 in if inside sa orig || shadow sa r d
    then rayint sb r d t tags
    else RayMiss

rayint_debug_bound :: Bound tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> (Rayint tag mat, Int)
rayint_debug_bound (Bound sa sb) r d t tags =
 let (Ray orig _) = r
 in if inside sa orig || shadow sa r d
    then (debug_wrap (rayint_debug sb r d t tags) 1)
    else (RayMiss,0)

shadow_bound :: Bound t m -> Ray -> Flt -> Bool
shadow_bound (Bound sa sb) r d =
 let (Ray orig _ ) = r
 in if inside sa orig || shadow sa r d
    then shadow sb r d
    else False

inside_bound :: Bound t m -> Vec -> Bool
inside_bound (Bound sa sb) pt = inside sa pt && inside sb pt

get_metainfo_bound :: Bound t m -> Vec -> ([Texture t m], [t])
get_metainfo_bound (Bound sa sb) v =
  if inside sa v
  then get_metainfo sb v
  else ([],[])

-- if this is too slow, we could just take the bounding box for sa
bound_bound :: Bound t m -> Bbox
bound_bound (Bound sa sb) = bboverlap (bound sa) (bound sb)

-- remove bounding objects when we flatten transformations
-- (this is so that the accelleration structure can 
-- build an automatic bounding hierarchy rather than
-- a manual one)

transform_leaf_bound :: Bound t m -> [Xfm] -> SolidItem t m
transform_leaf_bound (Bound sa sb) xfms =
 transform_leaf sb xfms

flatten_transform_bound :: Bound t m -> [SolidItem t m]
flatten_transform_bound (Bound sa sb) = flatten_transform sb

primcount_bound :: Bound t m -> Pcount
primcount_bound (Bound sa sb) = pcadd (asbound (primcount sa)) (primcount sb)

instance Solid (Bound t m) t m where
 rayint = rayint_bound
 rayint_debug = rayint_debug_bound
 shadow = shadow_bound
 inside = inside_bound
 bound = bound_bound
 flatten_transform = flatten_transform_bound
 transform_leaf = transform_leaf_bound
 primcount = primcount_bound
 get_metainfo = get_metainfo_bound
