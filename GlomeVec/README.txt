This is the vector library used by the Glome raytracer.  It has been separated 
out of the main Glome distribution as the beginning of an effort to separate 
all of the separately-useful modules in Glome.  (I have not yet removed the
Vec library from the main Glome distribution.)

This library may prove useful for graphics and computational geometry
algorithms.  It includes basic operations such as dot product, adding vectors,
etc, but it also includes transformations matricies and some useful operations
on axis-aligned bounding boxes such as clipping a ray to an AABB.   

See the Glome tutorial on the haskell wiki for details.

This was one of the first things I wrote in Haskell.  As such, it has a few
rough edges.

http://www.haskell.org/haskellwiki/Glome

Direct all questions to:
Jim Snow
jsnow@cs.pdx.edu
