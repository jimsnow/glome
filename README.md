glome
=====

Glome is a raytracer written in Haskell.

There are three packages here.

GlomeVec is a simple 3d vector math library.

GlomeTrace is the ray tracing engine itself.  It includes support for many basic primitives, including triangles, spheres, boxes, cylinders, cones, and planes.  It also has some composite primitives, such as CSG difference and intersection, which can be used to create more complex shapes out of the base primitives.

There is also a BIH-based acceleration structure that can be used to construct a tree of bounding volumes around any group of primitives.  This allows us to efficiently render scenes with a very large number of primitives.

GlomeView is a a simple program which renders a scene into an SDL window.  This is kept separate from GlomeTrace so that the raytracing engine can be used as a library without having any dependencies on SDL.

The typical way to use GlomeView is to edit the file TestScene.hs and then re-compile GlomeView.  Having an scene data format would be nice, but Defining scenes directly in Haskell can have some benefits too.  Most of the primitives have easy-to-use constructors that behave a lot like the equivalent primitives in POV-Ray.

GlomeView can use multiple cores with "+RTS -N4" or whatever.  It's not quite fast enough to be a useable replacement for OpenGL; I seem to be getting in the neighborhood of about 0.5-1fps at 720x480 with reasonably complex scenes.

More information can be found at the Haskell wiki, and the latest stable versions can be found on hackage.

http://www.haskell.org/haskellwiki/Glome
http://hackage.haskell.org/package/GlomeVec
http://hackage.haskell.org/package/GlomeTrace
http://hackage.haskell.org/package/GlomeView

