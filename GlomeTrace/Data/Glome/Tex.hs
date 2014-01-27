module Data.Glome.Tex (tex) where
import Data.Glome.Vec
import Data.Glome.Solid

-- Textured objects --
-- The type "Texture" is used elsewhere, so
-- we just call a textured object a "Tex".

-- How textured objects work is a little strange:
-- instead of having a texture applied to every object,
-- which seems rather wastefull, we use the container 
-- object "Tex"; anything contained in that Tex has 
-- that texture.

-- In the case of nested Tex objects, the innermost 
-- texture has precedence.  Textures are implemented
-- by passing a Texture in to the rayint function.
-- Most objects just return the Texture unchanged (as
-- part of the RayHit record) but Tex overwrites the 
-- texture with its own.

data Tex = Tex SolidItem Texture deriving Show

-- | Associate a texture with an object.  For composite
-- objects, the shader uses the innermost texture.
tex :: SolidItem -> Texture -> SolidItem
tex s t = SolidItem $ Tex s t

rayint_tex :: Tex -> Ray -> Flt -> Texture -> Rayint
rayint_tex (Tex s tex) r d t = rayint s r d tex

rayint_debug_tex :: Tex -> Ray -> Flt -> Texture -> (Rayint,Int)
rayint_debug_tex (Tex s tex) r d t = rayint_debug s r d tex

packetint_tex :: Tex -> Ray -> Ray -> Ray -> Ray -> Flt -> Texture -> PacketResult
packetint_tex (Tex s tx) r1 r2 r3 r4 d t = packetint s r1 r2 r3 r4 d tx

shadow_tex :: Tex -> Ray -> Flt -> Bool
shadow_tex (Tex s _) r d = shadow s r d

inside_tex :: Tex -> Vec -> Bool
inside_tex (Tex s _) pt = inside s pt

bound_tex :: Tex -> Bbox 
bound_tex (Tex s _) = bound s

primcount_tex :: Tex -> Pcount
primcount_tex (Tex s _) = primcount s

instance Solid Tex where
 rayint = rayint_tex
 rayint_debug = rayint_debug_tex
 packetint = packetint_tex
 shadow = shadow_tex
 inside = inside_tex
 bound = bound_tex
 primcount = primcount_tex
