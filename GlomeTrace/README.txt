This is GlomeTrace, a ray tracing library.  Originally, it was part of Glome-hs, my haskell ray tracer.  I decided to pull out the code to trace rays against objects and distribute it as a stand-alone library so that other projects can use it.

A good source of documentation is the Haskell wiki.  In particular, take a look at the tutorial I wrote for Glome-hs.  A few things have changed, but most of the descriptions there are still mostly valid.

If you're installing from a tarball, the usual commands will suffice:
runhaskell Setup configure; runhaskell Setup build; runhaskell Setup install

Note that GlomeTrace requires GlomeVec, a vector math library.

http://www.haskell.org/haskellwiki/Glome

Direct all questions to:
Jim Snow
jsnow@cs.pdx.edu
