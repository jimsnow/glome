module Data.Glome.Csg (difference, intersection) where
import Data.Glome.Vec
import Data.Glome.Solid
import Data.List

-- Constructive Solid Geometry
-- (boolean operations for solids)

-- todo: implement shadow tests

data Difference = Difference SolidItem SolidItem deriving Show
data Intersection = Intersection [SolidItem] deriving Show

--Difference--
-- | Create a new object based on the subtraction of the second item
-- from the first.  This only works if the items have a well-defined
-- inside and outside.  Triangles and discs, for instance, have no 
-- volume, so subtracting them from anything won't do anything.
difference :: SolidItem -> SolidItem -> SolidItem
difference a b = SolidItem $ Difference a b

{-
rayint_difference :: Difference -> Ray -> Flt -> Texture -> Rayint
rayint_difference dif@(Difference sa sb) r@(Ray orig dir) d t =
 let ria = rayint sa r d t
 in
  case ria of
   RayMiss -> RayMiss
   RayHit ad ap an at ->
    if inside sb orig 
    then
     case rayint sb r d t of
      RayMiss -> RayMiss 
      RayHit bd bp bn bt ->
       if bd < ad 
       then if inside sa bp 
            then RayHit bd bp (vinvert bn) bt
            else rayint_advance (SolidItem dif) r d t bd
       else rayint_advance (SolidItem dif) r d t bd
    else 
     if inside sb ap
     then rayint_advance (SolidItem dif) r d t ad
     else RayHit ad ap an at
-}

{-
allints :: SolidItem -> Ray -> Flt -> Texture -> [Rayint]
allints s r d t =
  case int of
    RayHit d p n t ->
      
    _ -> []
  where
    int = rayint s r d tt 

rayint_difference :: Difference -> Ray -> Flt -> Texture -> Rayint
rayint_difference (Difference sa sb) r@(Ray orig dir) d t =
  where

   inta = 
   intb =


rayint_difference :: Difference -> Ray -> Flt -> Texture -> Rayint
rayint_difference (Difference sa sb) r@(Ray orig dir) d t
  | (fabs $ (vlen dir)-1) > delta = error $ "bad direction vector " ++ (show r)
  | otherwise = go r d t
  where
    go r@(Ray orig dir) =
     if inside sb (vscaleadd orig dir (delta*0.5))
     then go_insideb r
     else go_outsideb r

    go_outsideb r d t =
      let ria = rayint sa r d t
      in case ria of
        RayHit ad ap an at ->
          
        miss -> miss

rayint_difference :: Difference -> Ray -> Flt -> Texture -> Rayint
rayint_difference dif@(Difference sa sb) r@(Ray orig dir) d t 
  | (fabs $ (vlen dir)-1) > delta = error $ "bad direction vector " ++ (show r)
  | otherwise = go r d t
  where
    go r@(Ray orig dir) d t =
     if inside sb (vscaleadd orig dir delta)
     then go_insideb r d t
     else go_outsideb r d t

    go_insideb r d t =
     let rib = rayint sb r d t
     in 
      case rib of
        RayHit bd bp bn bt -> 
          if inside sa bp && (not (inside sb (vscaleadd bp dir delta)))
          then RayHit bd bp (vinvert bn) bt
          else 
            case go (ray_move r (bd+delta)) (d-(bd+delta)) t of
              RayHit d' p' n' t' -> RayHit (d'+(bd+delta)) p' n' t'
              miss -> miss
        miss -> miss

    go_outsideb r d t =
     let ria = rayint sa r d t 
     in
      case ria of
        RayHit ad ap an at ->
          if inside sb ap
          then
            case go (ray_move r (ad+delta)) (d-(ad+delta)) t of
              RayHit d' p' n' t' -> RayHit (d'+(ad+delta)) p' n' t'
              miss -> miss
          else ria
        miss -> miss
-}

rayint_difference :: Difference -> Ray -> Flt -> Texture -> Rayint
rayint_difference dif@(Difference sa sb) r@(Ray orig dir) d t
  | inside sb orig =
      case rayint sb r d t of
        rib@(RayHit bd bp bn bt) ->
          if inside sa bp && (not (inside sb (vscaleadd bp dir delta)))
          then RayHit bd bp (vinvert bn) bt
          else rayint_advance (SolidItem dif) r d t bd
        miss -> miss
  | otherwise =
      case rayint sa r d t of
        ria@(RayHit ad ap an at) ->
          case rayint sb r d t of
            rib@(RayHit bd bp bn bt) ->
              if (ad < bd)
              then ria
              else rayint_advance (SolidItem dif) r d t bd
            RayMiss -> ria 
        miss -> miss


{-

rayint_difference :: Difference -> Ray -> Flt -> Texture -> Rayint
rayint_difference dif@(Difference sa sb) r@(Ray orig dir) d t =
 let ria = rayint sa r d t
 in
  case ria of
   RayMiss -> RayMiss
   RayHit ad ap an at ->
    if inside sb orig 
    then
     case rayint sb r d t of
      RayMiss -> RayMiss 
      RayHit bd bp bn bt ->
       if bd < ad 
       then if inside sa bp 
            then RayHit bd bp (vinvert bn) t
            else rayint_advance (SolidItem dif) r d t bd
       else rayint_advance (SolidItem dif) r d t bd
    else 
     if inside sb ap
     then rayint_advance (SolidItem dif) r d t ad
     else RayHit ad ap an at
-}

--Intersection--

-- | Create a new item from the boolean intersection of a
-- list of solids.  A point is inside the object iff it is
-- inside every primitive.  We can construct polyhedra from
-- intersections of planes, but this isn't the most efficient
-- way to do that.
intersection :: [SolidItem] -> SolidItem
intersection slds = SolidItem $ Intersection slds

-- fixme: there's some numerical instability near edges
rayint_intersection :: Intersection -> Ray -> Flt -> Texture -> Rayint
rayint_intersection (Intersection slds) r@(Ray orig dir) d t =
  if null slds || d < 0
  then RayMiss
  else 
   let s = head slds in
     case tail slds of
       [] -> rayint s r d t
       ss -> if inside s orig
             then case rayint s r d t of 
                   RayMiss -> rayint (Intersection ss) r d t
                   RayHit sd sp sn st -> 
                    case rayint (Intersection ss) r sd t of
                     RayMiss -> rayint_advance (SolidItem (Intersection slds)) 
                                               r d t sd 
                     hit -> hit
             else case rayint s r d t of
                   RayMiss -> RayMiss
                   RayHit sd sp sn st ->
                    if inside (Intersection ss) sp
                    then RayHit sd sp sn st
                    else rayint_advance (SolidItem (Intersection slds))
                                        r d t sd

inside_difference :: Difference -> Vec -> Bool
inside_difference (Difference sa sb) pt =
 (inside sa pt) && (not $ inside sb pt)

-- note: inside is True for an empty intersection.
-- this is actually the preferred semantics in 
-- some cases, strange as it may seem.
inside_intersection :: Intersection -> Vec -> Bool
inside_intersection (Intersection slds) pt =
 foldl' (&&) True (map (\x -> inside x pt) slds) 

bound_difference :: Difference -> Bbox
bound_difference (Difference sa sb) = bound sa

bound_intersection :: Intersection -> Bbox
bound_intersection (Intersection slds) =
 if null slds 
 then empty_bbox
 else foldl' bboverlap everything_bbox (map bound slds)

primcount_difference :: Difference -> Pcount
primcount_difference (Difference sa sb) = pcadd (primcount sa) (primcount sb)

primcount_intersection :: Intersection -> Pcount
primcount_intersection (Intersection slds) = foldl (pcadd) pcnone (map primcount slds)

instance Solid Difference where
 rayint = rayint_difference
 inside = inside_difference
 bound  = bound_difference
 primcount = primcount_difference

instance Solid Intersection where
 rayint = rayint_intersection
 inside = inside_intersection
 bound  = bound_intersection
 primcount = primcount_intersection
