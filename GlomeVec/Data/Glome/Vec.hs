{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

module Data.Glome.Vec where

-- | Performance is pretty similar with Floats or Doubles.
-- Todo: make separate Float and Double instances of this library.
type Flt = Double

-- maybe this is defined somewhere?
infinity :: Flt
--infinity = 1.0 / 0.0
infinity = 1000000.0

-- | Convert from degrees to native angle format (radians).
deg :: Flt -> Flt
deg !x = (x*3.1415926535897)/180

-- | Convert from radians to native format (noop).
rad :: Flt -> Flt
rad !x = x

-- | Convert from rotations to native format.  (rot 1 == deg 360)
rot :: Flt -> Flt
rot !x = x*3.1415926535897*2

-- | Trig with degrees instead of radians.
dcos :: Flt -> Flt
dcos d = cos $ deg d

-- | Force a value to be within a range.  Usage: clamp min x max
clamp :: Flt -> Flt -> Flt -> Flt
clamp !min !x !max
 | x < min = min
 | x > max = max
 | otherwise = x

-- | Tuning parameter.
delta = 0.0001 :: Flt

-- | Non-polymorphic fmin; this speeds
-- things up in ocaml, not sure about haskell.
fmin :: Flt -> Flt -> Flt
fmin !a !b = if a > b then b else a

-- | Non-polymorphic fmax.
fmax :: Flt -> Flt -> Flt
fmax !a !b = if a > b then a else b

-- | Non-polymorphic min of 3 values.
fmin3 :: Flt -> Flt -> Flt -> Flt
fmin3 !a !b !c = if a > b 
                 then if b > c 
                      then c
                      else b
                 else if a > c
                      then c
                      else a

-- | Non-polymorphic max of 3 values.
fmax3 :: Flt -> Flt -> Flt -> Flt
fmax3 !a !b !c = if a > b
                 then if a > c
                      then a
                      else c
                 else if b > c
                      then b
                      else c

-- | Min of 4 values.
fmin4 :: Flt -> Flt -> Flt -> Flt -> Flt
fmin4 !a !b !c !d = fmin (fmin a b) (fmin c d)

-- | Max of 4 values.
fmax4 :: Flt -> Flt -> Flt -> Flt -> Flt
fmax4 !a !b !c !d = fmax (fmax a b) (fmax c d)

-- | Non-polymorphic absolute value.
fabs :: Flt -> Flt
fabs !a = 
 if a < 0 then (-a) else a

-- | Non-polymorphic integer absolute value.
iabs :: Int -> Int
iabs !a =
 if a < 0 then (-a) else a

-- | Force user to use fabs or iabs, for performance reasons.  Not sure if
-- this really helps, though.
abs a = error "use non-polymorphic version, fabs"

-- | Approximate equality for Flt.  True if a and b are "almost" equal.
-- The (abs $ a-b) test doesn't work if
-- a and b are large.
about_equal :: Flt -> Flt -> Bool
about_equal !a !b =
 if a > 1 
 then
  fabs (1 - (a/b)) < (delta*10) 
 else
  (fabs $ a - b) < (delta*10)

-- | 3d type represented as a record of unboxed floats.
data Vec = Vec !Flt !Flt !Flt deriving Show

-- | A Ray is made up of an origin and direction Vec.
data Ray = Ray {origin, dir :: !Vec} deriving Show
--data Plane = Plane {norm :: !Vec, offset :: !Flt} deriving Show

ray_ub :: Ray -> (# Flt, Flt, Flt, Flt, Flt, Flt #)
ray_ub (Ray (Vec !ox !oy !oz) (Vec !dx !dy !dz)) = (# ox, oy, oz, dx, dy, dz #)


-- | Vec constructor.
vec :: Flt -> Flt -> Flt -> Vec
vec !x !y !z = (Vec x y z)

-- | Zero Vec.
vzero :: Vec
vzero = Vec 0.0 0.0 0.0

-- | For when we need a unit vector, but we 
-- don't care where it points.
vunit :: Vec
vunit = vx

-- | Unit X vector.
vx :: Vec
vx  = Vec 1 0 0

-- | Unit y vector.
vy :: Vec
vy  = Vec 0 1 0

-- | Unit z vector.
vz :: Vec
vz  = Vec 0 0 1

-- | Negative x vector.
nvx :: Vec
nvx = Vec (-1) 0 0

-- | Negative y vector.
nvy :: Vec
nvy = Vec 0 (-1) 0

-- | Negative z vector.
nvz :: Vec
nvz = Vec 0 0 (-1)

-- Extract x coordinate.
x :: Vec -> Flt
x (Vec x_ _ _) = x_

-- Extract y coordinate.
y :: Vec -> Flt
y (Vec _ y_ _) = y_

-- Extract z coordinate.
z :: Vec -> Flt
z (Vec _ _ z_) = z_

-- | Access the Vec as if it were an array indexed from 0..2.
-- Note: this actually accounts for a noticeable amount of cpu 
-- time in the Glome ray tracer.
va :: Vec -> Int -> Flt
va !(Vec x y z) !n = 
 case n of
  0 -> x
  1 -> y
  2 -> z

-- | Create a new Vec with the Nth field overwritten by new value.
-- I could have used record update syntax.
vset :: Vec -> Int -> Flt -> Vec
vset !(Vec x y z) !i !f =
 case i of
  0 -> Vec f y z
  1 -> Vec x f z
  2 -> Vec x y f

-- | Dot product of 2 vectors.  We use this all the time.  Dot product of 2
-- normal vectors is the cosine of the angle between them.
vdot :: Vec -> Vec -> Flt
vdot !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 (x1*x2)+(y1*y2)+(z1*z2)

-- | Cross product of 2 vectors.  Produces a vector perpendicular 
-- to the given vectors.  We use this for things like making the forward,
-- up, and right camera vectors orthogonal.  If the input vectors are
-- normalized, the output vector will be as well.
vcross :: Vec -> Vec -> Vec
vcross !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec 
  ((y1 * z2) - (z1 * y2))
  ((z1 * x2) - (x1 * z2))
  ((x1 * y2) - (y1 * x2))

-- | Apply a unary Flt operator to each field of the Vec.
vmap :: (Flt -> Flt) -> Vec -> Vec
vmap f !v1 = 
 Vec (f (x v1)) (f (y v1)) (f (z v1))

-- | Apply a binary Flt operator to pairs of fields from 2 Vecs.
vmap2 :: (Flt -> Flt -> Flt) -> Vec -> Vec -> Vec
vmap2 f !v1 !v2 =
 Vec (f (x v1) (x v2)) 
     (f (y v1) (y v2)) 
     (f (z v1) (z v2))

-- | Reverse the direction of a Vec.
vinvert :: Vec -> Vec
vinvert !(Vec x1 y1 z1) =
 Vec (-x1) (-y1) (-z1)

-- | Get the length of a Vec squared.  We use this to avoid a slow sqrt. 
vlensqr :: Vec -> Flt
vlensqr !v1 = vdot v1 v1

-- | Get the length of a Vec.  This is expensive because sqrt is slow.
vlen :: Vec -> Flt
vlen !v1 = sqrt (vdot v1 v1)

-- | Add 2 vectors.
vadd :: Vec -> Vec -> Vec
vadd !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec (x1 + x2)
     (y1 + y2)
     (z1 + z2)

-- | Add 3 vectors.
vadd3 :: Vec -> Vec -> Vec -> Vec
vadd3 !(Vec x1 y1 z1) !(Vec x2 y2 z2) !(Vec x3 y3 z3) =
    Vec (x1 + x2 + x3)
        (y1 + y2 + y3)
        (z1 + z2 + z3)

-- | Subtract vectors.  "vsub b a" is the vector from a to b.
vsub :: Vec -> Vec -> Vec
vsub !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec (x1 - x2)
     (y1 - y2)
     (z1 - z2)

-- | Multiply corresponding fields.  Rarely useful.
vmul :: Vec -> Vec -> Vec
vmul !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec (x1 * x2)
     (y1 * y2)
     (z1 * z2)

-- | Add a value to all the fields of a Vec.  Useful, for instance, to get
-- one corner of the bounding box around a sphere.
vinc :: Vec -> Flt -> Vec
vinc !(Vec x y z) !n =
 Vec (x + n)
     (y + n)
     (z + n)

-- | Subtract a value from all fields of a Vec.
vdec :: Vec -> Flt -> Vec
vdec !(Vec x y z) !n =
 Vec (x - n)
     (y - n)
     (z - n)

-- | Get the maximum of all corresponding fields between 2 Vecs.
vmax :: Vec -> Vec -> Vec
vmax !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec (fmax x1 x2)
     (fmax y1 y2)
     (fmax z1 z2)

-- | Get the minimum of all corresponding fields between 2 Vecs.
vmin :: Vec -> Vec -> Vec
vmin !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec (fmin x1 x2)
     (fmin y1 y2)
     (fmin z1 z2)

-- | Return the largest axis.  Often used with "va".
vmaxaxis :: Vec -> Int
vmaxaxis !(Vec x y z) =
 if (x > y) 
 then if (x > z) 
      then 0
      else 2
 else if (y > z) 
      then 1
      else 2

-- | Scale a Vec by some value.
vscale :: Vec -> Flt -> Vec
vscale !(Vec x y z) !fac =
 Vec (x * fac)
     (y * fac)
     (z * fac)

-- | Take the first Vec, and add to it the second Vec scaled by some amount.
-- This is used quite a lot in Glome.
vscaleadd :: Vec -> Vec -> Flt -> Vec
vscaleadd !(Vec x1 y1 z1) !(Vec x2 y2 z2) fac =
 Vec (x1 + (x2 * fac))
     (y1 + (y2 * fac))
     (z1 + (z2 * fac))
            
-- | Make the length of a Vec just a little shorter.
vnudge :: Vec -> Vec
vnudge x = vscale x (1-delta)

-- | Normalize a vector.  Division is expensive, so we compute the reciprocol 
-- of the length and multiply by that.  The sqrt is also expensive.
vnorm :: Vec -> Vec
vnorm !(Vec x1 y1 z1) = 
 let !invlen = 1.0 / (sqrt ((x1*x1)+(y1*y1)+(z1*z1))) in
 Vec (x1*invlen) (y1*invlen) (z1*invlen)

-- | Throw an exception if a vector hasn't been normalized.
assert_norm :: Vec -> Vec
assert_norm v =
 let l = vdot v v
 in if l > (1+delta) 
    then error $ "vector too long" ++ (show v)
    else if l < (1-delta)
         then error $ "vector too short: " ++ (show v)
         else v

-- | Get the victor bisecting two other vectors (which ought to be the same
-- length).
bisect :: Vec -> Vec -> Vec
bisect !v1 !v2 = vnorm (vadd v1 v2)

-- | Distance between 2 vectors.
vdist :: Vec -> Vec -> Flt
vdist v1 v2 = 
 let d = vsub v2 v1 in vlen d

-- | Reflect a vector "v" off of a surface with normal "norm".
reflect :: Vec -> Vec -> Vec
reflect !v !norm =
  -- vadd v $ vscale norm $ (-2) * (vdot v norm)
  vscaleadd v norm $ (-2) * (vdot v norm)

-- | Reciprocol of all fields of a Vec.
vrcp :: Vec -> Vec
vrcp !(Vec x y z) =
 Vec (1/x) (1/y) (1/z)

-- | Test Vecs for approximate equality
veq :: Vec -> Vec -> Bool
veq !(Vec ax ay az) !(Vec bx by bz) =
 (about_equal ax bx) && (about_equal ay by) && (about_equal az bz)

-- | Test Vecs for matching sign on all fields.  Returns false if any value is
-- zero.  Used by packet tracing.
veqsign :: Vec -> Vec -> Bool
veqsign !(Vec ax ay az) !(Vec bx by bz) =
 ax*bx > 0 && ay*by > 0 && az*bz > 0

-- | Translate a ray's origin in ray's direction by d amount.
ray_move :: Ray -> Flt -> Ray
ray_move !(Ray orig dir) !d =
 (Ray (vscaleadd orig dir d) dir)

-- | Find a pair of orthogonal vectors to the one given.
orth :: Vec -> (Vec,Vec)
orth v1 =
 if about_equal (vdot v1 v1) 1
 then
  let x = (Vec 1 0 0)
      y = (Vec 0 1 0)
      dvx = vdot v1 x
      v2 = if dvx < 0.8 && dvx > (-0.8) -- don't want to cross with a
           then vnorm $ vcross v1 x     -- vector that's too similar
           else vnorm $ vcross v1 y
      v3 = vcross v1 v2
  in (v2,v3)
 else error $ "orth: unnormalized vector" ++ (show v1)

-- | Intersect a ray with a plane 
-- defined by a point "p" and a normal "norm".
-- (Ray does not need to be normalized.)
plane_int :: Ray -> Vec -> Vec -> Vec
plane_int !(Ray orig dir) !p !norm =
 let newo = vsub orig p
     dist = -(vdot norm newo) / (vdot norm dir)
 in vscaleadd orig dir dist

-- | Find the distance along a ray until it intersects with a plane defined
-- by a point "p" and normal "norm".
plane_int_dist :: Ray -> Vec -> Vec -> Flt
plane_int_dist !(Ray orig dir) !p !norm =
 let newo = vsub orig p
 in -(vdot norm newo) / (vdot norm dir)

-- find intersection with plane
-- from graphics gems -- an efficient ray-polygon intersection
-- it seems that the ray need not be normalized
-- let plane_intersect ray (n,d) =
--  let t = -.((d +. (vdot n ray.origin)) /. (vdot n ray.dir))
--  in vadd ray.origin (vscale ray.dir t)


-- TRANSFORMATIONS --

-- | 3x4 Transformation matrix.  These are described in most graphics texts.
data Matrix = Matrix !Flt !Flt !Flt !Flt  
                     !Flt !Flt !Flt !Flt  
                     !Flt !Flt !Flt !Flt deriving Show

-- | A transformation.  Inverting a matrix is expensive, so we keep a forward
-- transformation matrix and a reverse transformation matrix.
-- Note: This can be made a little faster if the matricies are non-strict.
data Xfm = Xfm Matrix Matrix deriving Show

-- | Identity matrix.  Transforming a vector by this matrix does nothing.
ident_matrix :: Matrix
ident_matrix = (Matrix 1 0 0 0  0 1 0 0  0 0 1 0)

-- | Identity transformation.
ident_xfm :: Xfm
ident_xfm = Xfm ident_matrix ident_matrix

-- | Multiply two matricies.  This is unrolled for efficiency, and it's also
-- a little bit easier (in my opinion) to see what's going on.
mat_mult :: Matrix -> Matrix -> Matrix
mat_mult (Matrix a00 a01 a02 a03  a10 a11 a12 a13  a20 a21 a22 a23)
         (Matrix b00 b01 b02 b03  b10 b11 b12 b13  b20 b21 b22 b23) =
 Matrix
   (a00*b00 + a01*b10 + a02*b20)
   (a00*b01 + a01*b11 + a02*b21)
   (a00*b02 + a01*b12 + a02*b22)
   (a00*b03 + a01*b13 + a02*b23 + a03)

   (a10*b00 + a11*b10 + a12*b20)
   (a10*b01 + a11*b11 + a12*b21)
   (a10*b02 + a11*b12 + a12*b22)
   (a10*b03 + a11*b13 + a12*b23 + a13)

   (a20*b00 + a21*b10 + a22*b20)
   (a20*b01 + a21*b11 + a22*b21)
   (a20*b02 + a21*b12 + a22*b22)
   (a20*b03 + a21*b13 + a22*b23 + a23)

-- | Multiply two tranformations.  This just multiplies the forward and 
-- reverse transformations.
xfm_mult :: Xfm -> Xfm -> Xfm
xfm_mult (Xfm a inva) (Xfm b invb) =
 Xfm (mat_mult a b) (mat_mult invb inva)

-- TRANSFORM UTILITY FUNCTIONS --

-- | There is a seemingly-magical property of transformation matricies, that
-- we can combine the effects of any number of transformations into a single
-- transformation just by multiplying them together in reverse order.  For 
-- instance, we could move a point, then rotate it about the origin by some 
-- angle around some vector, then move it again, and this can all be done by 
-- a single transformation.
-- This function combines transformations in this way, though it reverses the
-- list first so the transformations take effect in their expected order.
compose :: [Xfm] -> Xfm
compose xfms = check_xfm $ foldr xfm_mult ident_xfm (reverse xfms)

-- | Make sure a transformation is valid.  Multipy the forward and reverse
-- matrix and verify that the result is the identity matrix.
check_xfm :: Xfm -> Xfm
check_xfm (Xfm m i) = 
 let (Matrix m00 m01 m02 m03  
             m10 m11 m12 m13  
             m20 m21 m22 m23) = mat_mult m i
     ae = about_equal
 in
  if ae m00 1 && ae m01 0 && ae m02 0 && ae m03 0 &&
     ae m10 0 && ae m11 1 && ae m12 0 && ae m13 0 &&
     ae m20 0 && ae m21 0 && ae m22 1 && ae m23 0
  then (Xfm m i)
  else error $ "corrupt matrix " ++ (show (Xfm m i)) ++ "\n" ++ (show (mat_mult m i)) 

-- | Complex transformations: Rotate point (or vector) "pt" about ray by 
-- angle c.  The angle is in radians,
-- but using the angle conversion routines "deg", "rad" and "rot" is 
-- recommended.
vrotate :: Vec -> Ray -> Flt -> Vec
vrotate pt (Ray orig axis_) angle =
 let axis = assert_norm axis_
     transform = compose [ translate (vinvert orig)
                         , rotate axis angle
                         , translate orig
                         ]
     new_pt = xfm_point transform pt
 in if about_equal (vlen (vsub orig pt)) (vlen (vsub orig new_pt))
    then new_pt
    else error $ "something is wrong with vrotate" ++ 
                 (show $ vlen (vsub orig pt)) ++ " " ++ 
                 (show $ vlen (vsub orig new_pt))


-- TRANSFORM APPLICATION --
-- these need to be fast

-- | Transform a point.  The point is treated as (x y z 1).
xfm_point :: Xfm -> Vec -> Vec
xfm_point !(Xfm (Matrix m00 m01 m02 m03  
                        m10 m11 m12 m13  
                        m20 m21 m22 m23) inv) 
          !(Vec x y z) =
 Vec (m00*x + m01*y + m02*z + m03)
     (m10*x + m11*y + m12*z + m13)
     (m20*x + m21*y + m22*z + m23)

-- | Inverse transform a point.
invxfm_point :: Xfm -> Vec -> Vec
invxfm_point !(Xfm fwd (Matrix i00 i01 i02 i03  
                               i10 i11 i12 i13  
                               i20 i21 i22 i23)) 
             !(Vec x y z) =
  Vec (i00*x + i01*y + i02*z + i03)
      (i10*x + i11*y + i12*z + i13)
      (i20*x + i21*y + i22*z + i23)

-- | Transform a vector.  The vector is treated as (x y z 0).
xfm_vec :: Xfm -> Vec -> Vec
xfm_vec !(Xfm (Matrix m00 m01 m02 m03  
                      m10 m11 m12 m13  
                      m20 m21 m22 m23) inv) 
        !(Vec x y z) =
 Vec (m00*x + m01*y + m02*z)
     (m10*x + m11*y + m12*z)
     (m20*x + m21*y + m22*z)

-- | Inverse transform a vector.
invxfm_vec :: Xfm -> Vec -> Vec
invxfm_vec !(Xfm fwd (Matrix i00 i01 i02 i03  
                             i10 i11 i12 i13  
                             i20 i21 i22 i23)) 
           !(Vec x y z) =
  Vec (i00*x + i01*y + i02*z)
      (i10*x + i11*y + i12*z)
      (i20*x + i21*y + i22*z)

-- | Inverse transform a normal.  This one is tricky: we need to transform 
-- by the inverse transpose.
invxfm_norm :: Xfm -> Vec -> Vec
invxfm_norm !(Xfm fwd (Matrix i00 i01 i02 i03  
                              i10 i11 i12 i13  
                              i20 i21 i22 i23)) 
            !(Vec x y z) =
 Vec (i00*x + i10*y + i20*z)
     (i01*x + i11*y + i21*z)
     (i02*x + i12*y + i22*z)

-- | Transform a Ray.
xfm_ray :: Xfm -> Ray -> Ray
xfm_ray !xfm !(Ray orig dir) =
 Ray (xfm_point xfm orig) (vnorm (xfm_vec xfm dir))

-- | Inverse transform a Ray.
invxfm_ray :: Xfm -> Ray -> Ray
invxfm_ray !xfm !(Ray orig dir) =
 Ray (invxfm_point xfm orig) (vnorm (invxfm_vec xfm dir))

-- BASIC TRANSFORMS --
-- | Basic transforms: move by some displacement vector.
translate :: Vec -> Xfm
translate (Vec x y z) =
 check_xfm $ Xfm (Matrix 1 0 0   x   0 1 0   y   0 0 1   z) 
                 (Matrix 1 0 0 (-x)  0 1 0 (-y)  0 0 1 (-z))

-- | Basic transforms: stretch along the three axes, by the amount
-- in the given vector.  (If x==y==z, then it's uniform scaling.)
scale :: Vec -> Xfm
scale (Vec x y z) =
 check_xfm $ Xfm (Matrix   x  0 0 0  0   y  0 0  0 0   z  0)
                (Matrix (1/x) 0 0 0  0 (1/y) 0 0  0 0 (1/z) 0)

-- | Basic transforms: rotate about a given axis by some angle.
rotate :: Vec -> Flt -> Xfm
rotate v@(Vec x y z) angle =
 if not $ (vlen v) `about_equal` 1
 then error $ "please use a normalized vector for rotation: " ++ (show (vlen v))
 else 
  let s = sin angle
      c = cos angle 

      m00 = ((x*x)+((1-(x*x))*c)) 
      m01 = (((x*y)*(1-c))-(z*s)) 
      m02 = ((x*z*(1-c))+(y*s))

      m10 = (((x*y)*(1-c))+(z*s))
      m11 = ((y*y)+((1-(y*y))*c))
      m12 = ((y*z*(1-c))-(x*s))

      m20 = ((x*z*(1-c))-(y*s))
      m21 = ((y*z*(1-c))+(x*s))
      m22 = ((z*z)+((1-(z*z))*c))
  in
  check_xfm $ Xfm (Matrix m00 m01 m02 0  m10 m11 m12 0  m20 m21 m22 0)
                  (Matrix m00 m10 m20 0  m01 m11 m21 0  m02 m12 m22 0)

-- | Basic transforms: Convert coordinate system from canonical xyz 
-- coordinates to uvw coordinates.
xyz_to_uvw :: Vec -> Vec -> Vec -> Xfm
xyz_to_uvw u v w =
 let Vec ux uy uz = u
     Vec vx vy vz = v
     Vec wx wy wz = w
 in if (vdot u u) `about_equal` 1
    then
     if (vdot v v) `about_equal` 1
     then
      if (vdot w w) `about_equal` 1
      then 
       if ((vdot u v) `about_equal` 0) && 
          ((vdot u w) `about_equal` 0) && 
          ((vdot v w) `about_equal` 0)
       then
        check_xfm $ Xfm (Matrix ux vx wx 0  uy vy wy 0  uz vz wz 0)
                        (Matrix ux uy uz 0  vx vy vz 0  wx wy wz 0)
       else error  "vectors aren't orthogonal"
      else error $ "unnormalized w " ++ (show w)
     else error $ "unnormalized v " ++ (show v)
    else error $ "unnormalized u " ++ (show u)

-- | Basic transforms: Convert from uvw coordinates back to normal xyz 
-- coordinates.
uvw_to_xyz :: Vec -> Vec -> Vec -> Xfm
uvw_to_xyz (Vec ux uy uz) (Vec vx vy vz) (Vec wx wy wz) =
 check_xfm $ Xfm (Matrix ux uy uz 0  vx vy vz 0  wx wy wz 0)
                 (Matrix ux vx wx 0  uy vy wy 0  uz vz wz 0)



-- TRIANGLE UTILITY FUNCTIONS --

-- | Given a side, angle, and side of a triangle, produce the length of the 
-- opposite side.
sas2s :: Flt -> Flt -> Flt -> Flt
sas2s s1 a s2 =
  sqrt (((s1 * s1) + (s2 * s2)) - ((2 * s1 * s2 * (dcos a))))



-- BOUNDING BOXES --
-- | Axis-aligned Bounding Box (AABB), defined by opposite corners.  P1 is the
-- min values, p2 has the max values.
data Bbox = Bbox {p1 :: !Vec, p2 :: !Vec} deriving Show

-- | A near-far pair of distances.  Basically just a tuple.
data Interval = Interval !Flt !Flt deriving Show -- used instead of a tuple

-- | Bounding box that encloses two bounding boxes.
bbjoin :: Bbox -> Bbox -> Bbox
bbjoin (Bbox p1a p2a) (Bbox p1b p2b) =
 (Bbox (vmin p1a p1b) (vmax p2a p2b))

-- | Find the overlap of two bounding boxes.
bboverlap :: Bbox -> Bbox -> Bbox
bboverlap (Bbox p1a p2a) (Bbox p1b p2b) =
 (Bbox (vmax p1a p1b) (vmin p2a p2b))

-- | Test if a Vec is inside the bounding box.
bbinside :: Bbox -> Vec -> Bool
bbinside (Bbox (Vec p1x p1y p1z) (Vec p2x p2y p2z)) (Vec x y z) =
 p1x <= x && x <= p2x && p1y <= y && y <= p2y && p1z <= z && z <= p2z

-- | Split a bounding box into two, given an axis and offset.  Throw exception
-- if the offset isn't inside the bounding box.
bbsplit :: Bbox -> Int -> Flt -> (Bbox,Bbox)
bbsplit (Bbox p1 p2) axis offset =
 if (offset < (va p1 axis)) || (offset > (va p2 axis))
 then error "degenerate bounding box split"
 else ((Bbox p1 (vset p2 axis offset)),
       (Bbox (vset p1 axis offset) p2))

-- | Generate a minimum bounding box that encloses a list of points.
bbpts :: [Vec] -> Bbox
bbpts [] = empty_bbox
bbpts ((Vec x y z):[]) =
 Bbox (Vec (x-delta) (y-delta) (z-delta)) 
      (Vec (x+delta) (y+delta) (z+delta))

bbpts ((Vec x y z):pts) =
 let (Bbox (Vec p1x p1y p1z) (Vec p2x p2y p2z)) = bbpts pts
     minx = fmin (x-delta) p1x
     miny = fmin (y-delta) p1y
     minz = fmin (z-delta) p1z
     maxx = fmax (x+delta) p2x
     maxy = fmax (y+delta) p2y
     maxz = fmax (z+delta) p2z in
 Bbox (Vec minx miny minz) (Vec maxx maxy maxz)

-- | Surface area of a bounding box.  Useful for cost heuristics when attempting
-- to build optimal bounding box heirarchies.  Undefined for degenerate bounding
-- boxes.
bbsa :: Bbox -> Flt
bbsa (Bbox p1 p2) =
 let Vec dx dy dz = vsub p2 p1 
 in dx*dy + dx*dz + dy*dz

-- | Volume of a bounding box.  Undefined for degenerate bounding boxes.
bbvol :: Bbox -> Flt
bbvol (Bbox p1 p2) =
 let (Vec dx dy dz) = vsub p2 p1
 in dx*dy*dz

-- | Degenerate bounding box that contains an empty volume.
empty_bbox :: Bbox
empty_bbox = 
 Bbox (Vec infinity infinity infinity) 
      (Vec (-infinity) (-infinity) (-infinity))

-- | "Infinite" bounding box.
everything_bbox :: Bbox
everything_bbox =
 Bbox (Vec (-infinity) (-infinity) (-infinity))
      (Vec infinity infinity infinity)

-- | Find a ray's entrance and exit from a bounding 
-- box.  If last entrance is before the first exit,
-- we hit.  Otherwise, we miss. (It's up to the 
-- caller to figure that out.)

{-
bbclip :: Ray -> Bbox -> Interval
bbclip (Ray (Vec ox oy oz) (Vec dx dy dz)) 
       (Bbox (Vec p1x p1y p1z) (Vec p2x p2y p2z)) =
 let !dxrcp = 1/dx
     !dyrcp = 1/dy
     !dzrcp = 1/dz
     Interval !inx !outx = if dx > 0 
                           then Interval ((p1x-ox)*dxrcp) ((p2x-ox)*dxrcp)
                           else Interval ((p2x-ox)*dxrcp) ((p1x-ox)*dxrcp)
     Interval !iny !outy = if dy > 0
                           then Interval ((p1y-oy)*dyrcp) ((p2y-oy)*dyrcp)
                           else Interval ((p2y-oy)*dyrcp) ((p1y-oy)*dyrcp)
     Interval !inz !outz = if dz > 0
                           then Interval ((p1z-oz)*dzrcp) ((p2z-oz)*dzrcp)
                           else Interval ((p2z-oz)*dzrcp) ((p1z-oz)*dzrcp)
 in
   Interval (fmax3 inx iny inz) (fmin3 outx outy outz)
-}

bbclip_ub :: Ray -> Bbox -> (# Flt, Flt #)
bbclip_ub (Ray (Vec !ox !oy !oz) (Vec !dx !dy !dz)) 
          (Bbox (Vec !p1x !p1y !p1z) (Vec !p2x !p2y !p2z)) =
 let !dxrcp = 1/dx
     !dyrcp = 1/dy
     !dzrcp = 1/dz
     (# !inx, !outx #) =
       if dx > 0 
       then (# (p1x-ox)*dxrcp, (p2x-ox)*dxrcp #)
       else (# (p2x-ox)*dxrcp, (p1x-ox)*dxrcp #)
     (# !iny, !outy #) =
       if dy > 0
       then (# (p1y-oy)*dyrcp, (p2y-oy)*dyrcp #)
       else (# (p2y-oy)*dyrcp, (p1y-oy)*dyrcp #)
     (# !inz, !outz #) =
       if dz > 0
       then (# (p1z-oz)*dzrcp, (p2z-oz)*dzrcp #)
       else (# (p2z-oz)*dzrcp, (p1z-oz)*dzrcp #)
 in
   (# fmax3 inx iny inz, fmin3 outx outy outz #)

bbclip :: Ray -> Bbox -> Interval
bbclip ray bbox =
  let (# near, far #) = bbclip_ub ray bbox
  in Interval near far
