module Data.Glome.Scene (
    Camera(Camera),
    camera,
    module Data.Glome.Clr,
    module Data.Glome.Vec,
    module Data.Glome.Solid,
    module Data.Glome.Sphere,
    module Data.Glome.Triangle,
    module Data.Glome.Bih,
    module Data.Glome.Csg,
    module Data.Glome.Plane,
    module Data.Glome.Box,
    module Data.Glome.Bound,
    module Data.Glome.Cone,
    module Data.Glome.Tex) where
import Data.Glome.Clr
import Data.Glome.Vec
import Data.Glome.Solid
import Data.Glome.Sphere
import Data.Glome.Triangle
import Data.Glome.Bih
import Data.Glome.Csg
import Data.Glome.Plane
import Data.Glome.Box
import Data.Glome.Bound
import Data.Glome.Cone
import Data.Glome.Tex

-- This is the proper module to import if you want to have
-- access to all the Solid constructors and scene
-- defininition code.


-- CAMERA --
data Camera = Camera {campos, fwd, up, right :: !Vec} 
              deriving Show

-- | Construct a camera pointing in some default direction.
default_cam = (Camera (vec 0.0 0.0 (-3.0)) 
                      (vec 0.0 0.0 1.0) 
                      (vec 0.0 1.0 0.0) 
                      (vec 1.0 0.0 0.0))

-- | Construct a camera, given a position, a forward vector, 
-- a point that the camera should be pointed towards, an up vector,
-- and a right vector.  The up and right vectors don't have to be
-- normalized or perfectly orthogonal.
camera :: Vec -> Vec -> Vec -> Flt -> Camera
camera pos at up angle =
 let fwd   = vnorm $ vsub at pos
     right = vnorm $ vcross up fwd
     up_   = vnorm $ vcross fwd right
     cam_scale = tan ((pi/180)*(angle/2))
 in
  Camera pos fwd
         (vscale up_ cam_scale) 
         (vscale right cam_scale)

