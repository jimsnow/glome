{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Glome.Csg (difference, intersection) where
import Data.Glome.Vec
import Data.Glome.Solid
import Data.List

-- Constructive Solid Geometry
-- (boolean operations for solids)

-- todo: implement shadow tests

data Difference t m = Difference (SolidItem t m) (SolidItem t m) Bool deriving Show
data Intersection t m = Intersection [SolidItem t m] deriving Show

--Difference--
-- | Create a new object based on the subtraction of the second item
-- from the first.  This only works if the items have a well-defined
-- inside and outside.  Triangles and discs, for instance, have no 
-- volume, so subtracting them from anything won't do anything.
--
-- If you use the "retexture" constructor, the surface hollowed
-- out by B will be rendered with B's texture, instead of A's texture.

difference :: SolidItem t m -> SolidItem t m -> SolidItem t m
difference a b = SolidItem $ Difference a b True

difference_retexture :: SolidItem t m -> SolidItem t m -> SolidItem t m
difference_retexture a b = SolidItem $ Difference a b False


rayint_difference :: Difference tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_difference dif@(Difference sa sb useatex) r@(Ray orig dir) d t tags
  | inside sb orig =
      case rayint sb r d t tags of
        rib@(RayHit bd bp bn ray uvw bt btags) ->
          if inside sa bp && (not (inside sb (vscaleadd bp dir delta)))
          then if useatex
               then let (atexs, atags) = get_metainfo sa bp
                    in RayHit bd bp (vinvert bn) ray uvw atexs atags
               else RayHit bd bp (vinvert bn) ray uvw bt btags
          else rayint_advance (SolidItem dif) r d t tags bd
        miss -> miss
  | otherwise =
      case rayint sa r d t tags of
        ria@(RayHit ad ap an aray auvw at atags) ->
          case rayint sb r d t tags of
            rib@(RayHit bd bp bn bray buvw bt btags) ->
              if (ad < bd)
              then ria
              else rayint_advance (SolidItem dif) r d t tags bd
            RayMiss -> ria 
        miss -> miss


--Intersection--

-- | Create a new item from the boolean intersection of a
-- list of solids.  A point is inside the object iff it is
-- inside every primitive.  We can construct polyhedra from
-- intersections of planes, but this isn't the most efficient
-- way to do that.
intersection :: [SolidItem tag mat] -> SolidItem tag mat
intersection slds = SolidItem $ Intersection slds

-- fixme: there's some numerical instability near edges
rayint_intersection :: Intersection tag mat -> Ray -> Flt -> [Texture tag mat] -> [tag] -> Rayint tag mat
rayint_intersection (Intersection slds) r@(Ray orig dir) d t tags =
  if null slds || d < 0
  then RayMiss
  else 
   let s = head slds in
     case tail slds of
       [] -> rayint s r d t tags
       ss -> if inside s orig
             then case rayint s r d t tags of 
                   RayMiss -> rayint (Intersection ss) r d t tags
                   RayHit sd sp sn sray suvw st stags -> 
                    case rayint (Intersection ss) r sd t tags of
                     RayMiss -> rayint_advance (SolidItem (Intersection slds)) 
                                               r d t tags sd 
                     hit -> hit
             else case rayint s r d t tags of
                   RayMiss -> RayMiss
                   RayHit sd sp sn sray suvw st stags ->
                    if inside (Intersection ss) sp
                    then RayHit sd sp sn r vzero st stags
                    else rayint_advance (SolidItem (Intersection slds))
                                        r d t tags sd

inside_difference :: Difference tag mat -> Vec -> Bool
inside_difference (Difference sa sb useatex) pt =
 (inside sa pt) && (not $ inside sb pt)

-- note: inside is True for an empty intersection.
-- this is actually the preferred semantics in 
-- some cases, strange as it may seem.
inside_intersection :: Intersection tag mat -> Vec -> Bool
inside_intersection (Intersection slds) pt =
 foldl' (&&) True (map (\x -> inside x pt) slds) 

get_metainfo_difference (Difference sa sb useatex) pt =
 if (inside sa pt) && (not $ inside sb pt)
 then get_metainfo sa pt
 else ([],[])

get_metainfo_intersection (Intersection slds) pt =
 if foldl' (&&) True (map (\x -> inside x pt) slds)
 then foldl' paircat ([],[]) $ map (\s -> get_metainfo s pt) slds
 else ([],[]) 

bound_difference :: Difference tag mat -> Bbox
bound_difference (Difference sa _ _) = bound sa

bound_intersection :: Intersection tag mat -> Bbox
bound_intersection (Intersection slds) =
 if null slds 
 then empty_bbox
 else foldl' bboverlap everything_bbox (map bound slds)

primcount_difference :: Difference t m -> Pcount
primcount_difference (Difference sa sb _) = pcadd (primcount sa) (primcount sb)

primcount_intersection :: Intersection t m -> Pcount
primcount_intersection (Intersection slds) = foldl (pcadd) pcnone (map primcount slds)

instance Solid (Difference t m) t m where
 rayint = rayint_difference
 inside = inside_difference
 bound  = bound_difference
 primcount = primcount_difference
 get_metainfo = get_metainfo_difference

instance Solid (Intersection t m) t m where
 rayint = rayint_intersection
 inside = inside_intersection
 bound  = bound_intersection
 primcount = primcount_intersection
 get_metainfo = get_metainfo_intersection
