{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Glome.Tex (tex, tag, noshadow, onlyshadow) where
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


data Tag tag mat = Tag (SolidItem tag mat) tag
data Tex tag mat = Tex (SolidItem tag mat) (Texture tag mat) deriving Show
data NoShadow tag mat = NoShadow (SolidItem tag mat) deriving Show
data OnlyShadow tag mat = OnlyShadow (SolidItem tag mat) deriving Show

-- | Associate a texture with an object.  For composite
-- objects, the shader uses the innermost texture.
tex :: SolidItem tag mat -> Texture tag mat -> SolidItem tag mat
tex s t = SolidItem $ Tex s t

-- | Associate a tag with an object.  These are returned in
-- ray intersection.
tag :: SolidItem tag mat -> tag -> SolidItem tag mat
tag s t = SolidItem $ Tag s t

-- | Supress shadows for the contained object, for performance
-- or artistic effect.
noshadow s = SolidItem $ NoShadow s

-- | Contained object casts a shadow, but can't be seen.
-- (May not work for primitives which implement a shadow test by
-- calling rayint.)
onlyshadow s = SolidItem $ OnlyShadow s

instance Show (Tag tag mat) where
  show (Tag s t) = "<Tag " ++ show s ++ ">"

instance Solid (Tag t m) t m where
 rayint       (Tag s tag) r d texs tags = rayint       s r d texs (tag:tags)
 rayint_debug (Tag s tag) r d texs tags = rayint_debug s r d texs (tag:tags)
 packetint (Tag s tag) r1 r2 r3 r4 d texs tags = packetint s r1 r2 r3 r4 d texs (tag:tags)
 shadow    (Tag s _) = shadow s
 inside    (Tag s _) = inside s
 bound     (Tag s _) = bound s
 primcount (Tag s _) = primcount s
 get_metainfo (Tag s tag) v = let (texs, tags) = get_metainfo s v
                              in (texs, tag:tags)


instance Solid (Tex t m) t m where
 rayint       (Tex s tex) r d texs tags = rayint       s r d (tex:texs) tags
 rayint_debug (Tex s tex) r d texs tags = rayint_debug s r d (tex:texs) tags
 packetint (Tex s tex) r1 r2 r3 r4 d texs tags = packetint s r1 r2 r3 r4 d (tex:texs) tags
 shadow    (Tex s _) = shadow s
 inside    (Tex s _) = inside s
 bound     (Tex s _) = bound s
 primcount (Tex s _) = primcount s
 get_metainfo (Tex s tex) v = let (texs, tags) = get_metainfo s v
                              in (tex:texs, tags)


instance Solid (NoShadow t m) t m where
 rayint       (NoShadow s) r d texs tags = rayint       s r d texs tags
 rayint_debug (NoShadow s) r d texs tags = rayint_debug s r d texs tags
 packetint    (NoShadow s) r1 r2 r3 r4 d texs tags = packetint s r1 r2 r3 r4 d texs tags
 shadow       (NoShadow s) r d = False
 inside       (NoShadow s) = inside s
 bound        (NoShadow s) = bound s
 primcount    (NoShadow s) = primcount s
 get_metainfo (NoShadow s) v = get_metainfo s v


instance Solid (OnlyShadow t m) t m where
 rayint       (OnlyShadow s) r d texs tags = RayMiss
 rayint_debug (OnlyShadow s) r d texs tags = (RayMiss,0)
 packetint    (OnlyShadow s) r1 r2 r3 r4 d texs tags = packetmiss
 shadow       (OnlyShadow s) = shadow s
 inside       (OnlyShadow s) = inside s
 bound        (OnlyShadow s) = bound s
 primcount    (OnlyShadow s) = primcount s
 get_metainfo (OnlyShadow s) v = get_metainfo s v




