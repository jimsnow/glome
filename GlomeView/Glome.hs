
import Control.DeepSeq(NFData, rnf)
import Control.Monad.Par
import Graphics.UI.SDL as SDL
import Control.Monad(forM_)
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.Time.Clock.POSIX

import Data.Vector.Generic.Base
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV

import Data.Glome.Scene as Scene
import Data.Glome.Trace as Trace
import Data.Glome.Spd as Spd
import TestScene


maxdepth = 3 -- recursion depth for reflection/refraction

get_rayint :: Flt -> Flt -> Scene -> (Scene.ColorA, [Tag], Rayint Tag M)
get_rayint x y scn = 
 let (sld, lights, (Camera pos fwd up right), shader) = scn
     dir = vnorm $ vadd3 fwd (vscale right (-x)) (vscale up y)
     ray = (Ray pos dir) 
 in
    Trace.trace lights shader sld ray infinity maxdepth


get_color x y scn =
  let (color, tags, ri) = get_rayint x y scn
  in (color, 0)                    

get_color' :: Flt -> Flt -> Scene -> (Scene.ColorA,Flt)
get_color' x y _ =
  (Scene.ColorA x y (x+y) 1, 0)


get_tags x y scn =
  let (color, tags, ri) = get_rayint x y scn
  in tags

-- compute a 2x2 packet of four rays from corners of box
{-
get_packet :: Flt -> Flt -> Flt -> Flt -> Scene -> PacketColor
get_packet x1 y1 x2 y2 scn =
 let (Scene sld lights (Camera pos fwd up right) dtex bgcolor) = scn
     dir1 = vnorm $ vadd3 fwd (vscale right (-x1)) (vscale up y1)
     dir2 = vnorm $ vadd3 fwd (vscale right (-x2)) (vscale up y1)
     dir3 = vnorm $ vadd3 fwd (vscale right (-x1)) (vscale up y2)
     dir4 = vnorm $ vadd3 fwd (vscale right (-x2)) (vscale up y2)
     ray1 = Ray pos dir1
     ray2 = Ray pos dir2
     ray3 = Ray pos dir3
     ray4 = Ray pos dir4
 in trace_packet scn ray1 ray2 ray3 ray4 infinity maxdepth
-}

cap1 :: Flt -> Flt
cap1 x
 | x >= 1 = 1-delta
 | otherwise = x      

-- mapRGB does this, but we need a pixel format...
rgb :: Int -> Int -> Int -> Pixel
rgb r g b = Pixel $ (fromIntegral r)*(256*256) + (fromIntegral g)*256 + (fromIntegral b)

rgbf :: Flt -> Flt -> Flt -> Pixel
rgbf r g b = Pixel $ (floor ((cap1 r)*256))*(256*256) +
                     (floor ((cap1 g)*256))*256 +
                     (floor ((cap1 b)*256))

xres = 720
yres = 480
blocksize = 64


-- from http://stackoverflow.com/questions/5443259/loading-a-opengl-image-with-sdl-image
setPixel32 :: Surface -> Int -> Int -> Pixel -> IO ()
setPixel32 surf x y (Pixel pixel) = do
  ps <- surfaceGetPixels surf
  if x >= 0 && x < surfaceGetWidth surf && y >= 0 && y < surfaceGetHeight surf
    then pokeElemOff (castPtr ps :: Ptr Word32) offset pixel
    else error "setPixel32 -- bad coordinate"
 where offset = y * (fromIntegral $ surfaceGetPitch surf `div` 4) + x


-- r g b debug index
data Tile = Tile Rect (UV.Vector (Flt, Flt, Flt, Flt))

instance NFData Tile where
  rnf (Tile r v) = rnf v


-- We pass in the Rect for the whole image, and a sub-Rect for the tile
-- we want to render.
-- We throw away the alpha value here.
renderTile :: Rect -> Rect -> Scene -> Tile
renderTile (Rect _ _ width height) t@(Rect xtmin ytmin twidth theight) scene =
  let widthf = fromIntegral width
      heightf = fromIntegral height
      gen i =
        let xf = fromIntegral $ xtmin + (mod i twidth)
            yf = fromIntegral $ ytmin + (div i twidth)
            xcoord = (((xf/widthf)*2)-1) * (widthf/heightf)
            ycoord = -(((yf/heightf)*2)-1)
            (ColorA r g b _, d) = get_color xcoord ycoord scene
            --(r,g,b,d) = (0.5, 0, 0, 0)
        in
          (r, g, b, d)
          
  in Tile t (UV.generate (twidth * theight) gen)

blitTile :: Surface -> Tile -> IO ()
blitTile surf (Tile (Rect xtmin ytmin twidth theight) pixels) =
  UV.zipWithM_ drawf (UV.fromList [0..((twidth*theight)-1)]) pixels
  where
    drawf i (r, g, b, d) = 
      setPixel32 surf (xtmin + (mod i twidth)) (ytmin + (div i twidth)) (rgbf r g b)


     {- do
      _ <- fillRect
             surf
             (Just (Rect (xtmin + (mod i twidth)) 
                         (ytmin + (div i twidth))  1 1))
             (rgbf r g b)
      return () -}

-- Given an Int, return a list of Ints broken down into blocksize chunks.
-- Each chunk is an (offset,size) tuple.
chunk size blocksize =
  go 0
    where
      go pos =
        if pos + blocksize >= size
        then [(pos, (size-pos))]
        else (pos, blocksize) : (go (pos+blocksize))

renderTiles :: Surface -> Scene -> Int -> IO ()
renderTiles surf scene blocksize = do
  srect@(Rect _ _ width height) <- getClipRect surf
  let xchunks = chunk width blocksize
  let ychunks = chunk height blocksize
  let blocks = [Rect xpos ypos width height   | (xpos, width) <- xchunks, (ypos, height) <- ychunks]
  let tiles = runPar $ parMap (\block -> renderTile srect block scene) blocks
  forM_ tiles (blitTile surf)
  

-- Simple version, not amenable to parallelization.
render :: Surface -> Scene -> IO ()
render surf scene = do
  Rect xmin ymin width height <- getClipRect surf
  let xmax = xmin+width
  let ymax = ymin+height
  forM_ [xmin..xmax] (\x ->
    forM_ [ymin..ymax] (\y ->
      let xf = fromIntegral x
          yf = fromIntegral y
          widthf = fromIntegral width
          heightf = fromIntegral height
          xcoord = (((xf/widthf)*2)-1) * (widthf/heightf)
          ycoord = -(((yf/heightf)*2)-1)
          (ColorA r g b _, d) = get_color xcoord ycoord scene
      in
        do _ <- fillRect surf (Just (Rect x y 1 1)) (rgbf r g b)
           return ()
                       )
                     )

getTags' x y scn =
  let xf = fromIntegral $ x
      yf = fromIntegral $ y
      widthf = fromIntegral xres
      heightf = fromIntegral yres
      xcoord = (((xf/widthf)*2)-1) * (widthf/heightf)
      ycoord = -(((yf/heightf)*2)-1)
  in  
      get_tags xcoord ycoord scn

runHandler :: Scene -> IO ()
runHandler scn = do
  e <- pollEvent
  --e <- waitEvent
  case e of
    Quit -> return ()
    MouseButtonUp x y ButtonLeft ->
      do let ts = getTags' x y scn
         print $ (show x) ++ " " ++ (show y) ++ ":"
         forM_ ts print
         runHandler scn
    NoEvent ->
      do delay 50
         runHandler scn
    otherwise -> 
      runHandler scn

main = do
  SDL.init [InitVideo, InitTimer, InitJoystick]
  setVideoMode xres yres 32 []
  screen <- getVideoSurface

  -- HWSurface seems to be slightly faster.
  img <- createRGBSurface [HWSurface] xres yres 32 0 0 0 0

  setupt1 <- getPOSIXTime
  scene@(geom, _, _, _) <- TestScene.scn

  print $ "(primitives,transforms,bounding objects): " ++ (show (primcount geom))
  setupt2 <- getPOSIXTime
  print $ "scene setup: " ++ (show (setupt2-setupt1))

  forM_ [1..1] (\_ ->
    do rendert1 <- getPOSIXTime
       renderTiles img scene blocksize
       rendert2 <- getPOSIXTime
       print $ "render: " ++ (show (rendert2-rendert1)))

  blitt1 <- getPOSIXTime
  blitSurface img Nothing screen Nothing
  SDL.flip screen
  blitt2 <- getPOSIXTime
  print $ "blit: " ++ (show (blitt2-blitt1))

  runHandler scene
