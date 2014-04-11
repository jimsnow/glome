{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DoAndIfThenElse #-}

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
import qualified Data.Vector.Unboxed.Mutable as MUV

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

get_rayint_debug :: Flt -> Flt -> Scene -> ((Scene.ColorA, [Tag], Rayint Tag M), Int)
get_rayint_debug x y scn = 
 let (sld, lights, (Camera pos fwd up right), shader) = scn
     dir = vnorm $ vadd3 fwd (vscale right (-x)) (vscale up y)
     ray = (Ray pos dir) 
 in
    Trace.trace_debug lights shader sld ray infinity maxdepth

get_packetint :: Flt -> Flt -> Flt -> Flt -> Scene -> PacketTraceResult Tag M
get_packetint xmin ymin xmax ymax scn =
  let (sld, lights, (Camera pos fwd up right), shader) = scn
      ray1 = Ray pos $ vnorm $ vadd3 fwd (vscale right (-xmin)) (vscale up ymin)
      ray2 = Ray pos $ vnorm $ vadd3 fwd (vscale right (-xmax)) (vscale up ymin)
      ray3 = Ray pos $ vnorm $ vadd3 fwd (vscale right (-xmin)) (vscale up ymax)
      ray4 = Ray pos $ vnorm $ vadd3 fwd (vscale right (-xmax)) (vscale up ymax)
  in
     Trace.trace_packet lights shader sld ray1 ray2 ray3 ray4 infinity maxdepth

get_color_normal x y scn =
  let (color, tags, ri) = get_rayint x y scn
  in (color, ridepth ri)           

get_color_debug x y scn =
  let ((ColorA r g b a, tags, ri), dbg)= get_rayint_debug x y scn
      dbgf = fromIntegral dbg
  in (ColorA ((fromIntegral (mod dbg 30) / 60) + r) (g+(dbgf / 1000)) b a, ridepth ri)

get_color = get_color_normal

-- test pattern
get_color' :: Flt -> Flt -> Scene -> (Scene.ColorA,Flt)
get_color' x y _ =
  (Scene.ColorA x y (x+y) 1, 0)

get_tags :: Flt -> Flt -> Scene -> [Tag]
get_tags x y scn =
  let (color, tags, ri) = get_rayint x y scn
  in tags

get_packetcolor :: Flt -> Flt -> Flt -> Flt -> Scene -> (Scene.ColorA, Scene.ColorA, Scene.ColorA, Scene.ColorA)
get_packetcolor xmin ymin xmax ymax scn =
  let PacketTraceResult (c1,_,_) (c2,_,_) (c3,_,_) (c4,_,_) =
        get_packetint xmin ymin xmax ymax scn
  in
     (c1,c2,c3,c4)


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

-- Due to the oddities of supersampling, it's better if this is an odd number.
blocksize = 65

-- Convert screen coordinates to camera coordinates.
getCoords :: Int -> Int -> Int -> Int -> (# Flt, Flt #) 
getCoords width height x y =
  let xf = fromIntegral $ x
      yf = fromIntegral $ y
      widthf = fromIntegral width
      heightf = fromIntegral height
      xcoord = (((xf/widthf)*2)-1) * (widthf/heightf)
      ycoord = -(((yf/heightf)*2)-1)
  in
    (# xcoord, ycoord #)

-- Convert screen coordinates to camera coodinates, where
-- screen coordinates are given as floats.  This is valuable
-- for when we need sub-pixel precision (e.g. for supersampling).
getCoordsf :: Int -> Int -> Flt -> Flt -> (# Flt, Flt #) 
getCoordsf width height xf yf =
  let widthf = fromIntegral width
      heightf = fromIntegral height
      xcoord = (((xf/widthf)*2)-1) * (widthf/heightf)
      ycoord = -(((yf/heightf)*2)-1)
  in
    (# xcoord, ycoord #)

-- from http://stackoverflow.com/questions/5443259/loading-a-opengl-image-with-sdl-image
setPixel32 :: Surface -> Int -> Int -> Pixel -> IO ()
setPixel32 surf x y (Pixel pixel) = do
  ps <- surfaceGetPixels surf
  if x >= 0 && x < surfaceGetWidth surf && y >= 0 && y < surfaceGetHeight surf
    then pokeElemOff (castPtr ps :: Ptr Word32) offset pixel
    else error "setPixel32 -- bad coordinate"
 where offset = y * (fromIntegral $ surfaceGetPitch surf `div` 4) + x


-- r g b alpha depth
type TColor = (Flt, Flt, Flt, Flt, Flt)
data Tile = Tile Rect (UV.Vector TColor)

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
            (ColorA r g b a, d) = get_color xcoord ycoord scene
            --(r,g,b,d) = (0.5, 0, 0, 0)
        in
          (r + (d / 400), g, b, a, d)
          
  in Tile t (UV.generate (twidth * theight) gen)


cCmp :: TColor -> TColor -> Flt 
cCmp (r1, g1, b1, a1, d1) (r2, g2, b2, a2, d2) = 
 (diff r1 r2) + (diff g1 g2) + (diff b1 b2) + (diff a1 a2) + (muldiff d1 d2)
  where
    diff a b = fabs (b-a)

    -- proportional difference, rather than absolute
    muldiff 0 0 = 0
    muldiff a b = if a > b
                  then (a/b) - 1
                  else (b/a) - 1

cAvg :: TColor -> TColor -> TColor -> TColor -> TColor
cAvg (r1, g1, b1, a1, d1) (r2, g2, b2, a2, d2) (r3, g3, b3, a3, d3) (r4, g4, b4, a4, d4) =
  ((r1 + r2 + r3 + r4) * 0.25,
   (g1 + g2 + g3 + g4) * 0.25,
   (b1 + b2 + b3 + b4) * 0.25,
   (a1 + a2 + a3 + a4) * 0.25,
   (d1 + d2 + d3 + d4) * 0.25)

cAvg2 :: TColor -> TColor -> TColor
cAvg2 (r1, g1, b1, a1, d1) (r2, g2, b2, a2, d2) =
  ((r1 + r2) * 0.5,
   (g1 + g2) * 0.5,
   (b1 + b2) * 0.5,
   (a1 + a2) * 0.5,
   (d1 + d2) * 0.5)

decide_int :: Flt -> Int -> Int -> Int -> Int -> Scene -> TColor -> TColor -> TColor -> TColor -> TColor
decide_int threshold width height x y scene a b c d =
  let (# xf, yf #) = getCoords width height x y
  in decide threshold xf yf scene a b c d

-- Four colors should be in order, CW or CCW.
decide :: Flt -> Flt -> Flt -> Scene -> TColor -> TColor -> TColor -> TColor -> TColor
decide threshold xf yf scene a b c d =
  let variance = fmax (cCmp a c) (cCmp b d)
  in if variance > threshold
     then let (ColorA r g b a, d) = get_color xf yf scene
          in (r, g, b, a, d)
     else cAvg a b c d

threshold1 = 0.14
threshold2 = 0.15
threshold3 = 0.16
threshold4 = 0.18

renderTileSubsample :: Rect -> Rect -> Scene -> Tile
renderTileSubsample (Rect _ _ width height) t@(Rect xtmin ytmin twidth theight) scene =
  let widthf = fromIntegral width
      heightf = fromIntegral height
  in Tile t (UV.create
              (do v <- MUV.replicate (twidth * theight) (0,0,0,0,infinity)

                  let getc x y = if (x >= xtmin) && (x < xtmin+twidth) && (y >= ytmin) && (y < ytmin+theight)
                                 then MUV.read v ((x-xtmin) + ((y-ytmin)*twidth))
                                 else return (0,0,0,0,infinity)

                  let putc v x y c = MUV.write v ((x-xtmin) + ((y-ytmin)*twidth)) c

                  if True
                  then
                    do forM_ [xtmin,xtmin+2..xtmin+twidth-1] (\x ->
                         forM_ [ytmin,ytmin+2..ytmin+theight-1] (\y ->
                           if mod ((x-xtmin) + (y-ytmin)) 4 == 0
                           then
                             do let (# xf, yf #) = getCoords width height x y
                                let (ColorA r g b a, d) = get_color xf yf scene
                                putc v x y (r, g, b, a, d)
                           else return ()
                           )
                         )
                       forM_ [xtmin,xtmin+2..xtmin+twidth-1] (\x ->
                         forM_ [ytmin,ytmin+2..ytmin+theight-1] (\y ->
                           if mod ((x-xtmin) + (y-ytmin)) 4 == 2
                           then
                             do a <- getc (x-2) y
                                b <- getc x (y+2)
                                c <- getc (x+2) y
                                d <- getc x (y-2)
                                putc v x y (decide_int threshold1 width height x y scene a b c d)
                           else return ()
                           )
                         )
                  else
                    forM_ [xtmin,xtmin+2..xtmin+twidth-1] (\x ->
                      forM_ [ytmin,ytmin+2..ytmin+theight-1] (\y ->
                        do let (# xf, yf #) = getCoords width height x y
                           let (ColorA r g b a, d) = get_color xf yf scene
                           putc v x y (r, g, b, a, d)
                        )
                      )

                  forM_ [xtmin+1,xtmin+3..xtmin+twidth-1] (\x ->
                    forM_ [ytmin+1,ytmin+3..ytmin+theight-1] (\y ->
                      do a <- getc (x-1) (y-1)
                         b <- getc (x+1) (y-1)
                         c <- getc (x+1) (y+1)
                         d <- getc (x-1) (y+1)
                         putc v x y (decide_int threshold2 width height x y scene a b c d)
                         return ()
                      )
                    )

                  forM_ [xtmin..xtmin+twidth-1] (\x ->
                    forM_ [ytmin..ytmin+theight-1] (\y ->
                      if mod ((x-xtmin) + (y-ytmin)) 2 == 1
                      then
                        do a <- getc (x-1) y
                           b <- getc x (y+1)
                           c <- getc (x+1) y
                           d <- getc x (y-1)
                           putc v x y (decide_int threshold3 width height x y scene a b c d)
                           return ()
                      else return ()
                      )
                    )

                  -- When we supersample, we need to overwrite existing values.
                  -- Therefore, we make a copy.
                  v2 <- MUV.replicate (twidth * theight) (0,0,0,0,infinity)
                  
                  forM_ [xtmin..xtmin+twidth-1] (\x ->
                    forM_ [ytmin..ytmin+theight-1] (\y ->
                        do a <- getc x y
                           b <- getc x (y+1)
                           c <- getc (x+1) (y+1)
                           d <- getc (x+1) y
                           let (# xf, yf #) = getCoordsf width height ((fromIntegral x) + 0.5) ((fromIntegral y) + 0.5)
                           let color = decide threshold4 xf yf scene a b c d
                           if x == xtmin + twidth - 1
                           then if y == ytmin + theight - 1
                                then putc v2 x y color
                                else putc v2 x y (cAvg2 color (cAvg2 a b))
                           else
                                if y == ytmin + theight - 1
                                then putc v2 x y (cAvg2 color (cAvg2 a d))
                                else putc v2 x y (cAvg2 color (cAvg a b c d))
                           return ()
                      )
                    )

                  return v2
              )
            )


colorConv :: ColorA -> (Flt, Flt, Flt, Flt, Flt)
colorConv (ColorA r g b a) = (r,g,b,a,0) 

renderTilePacket :: Rect -> Rect -> Scene -> Tile
renderTilePacket (Rect _ _ width height) t@(Rect xtmin ytmin twidth theight) scene =
  let widthf = fromIntegral width
      heightf = fromIntegral height

  in Tile t (UV.create
              (do v <- MUV.new (twidth * theight)
                  forM_ [xtmin,xtmin+2..xtmin+twidth-1] (\x ->
                    forM_ [ytmin,ytmin+2..ytmin+theight-1] (\y ->
                      do let (# minx, miny #) = getCoords width height x y
                         let (# maxx, maxy #) = getCoords width height (x+1) (y+1)
                         let (c1, c2, c3, c4) = get_packetcolor minx miny maxx maxy scene
                         let offset = (x-xtmin) + ((y-ytmin)*twidth)
                         MUV.write v offset (colorConv c1)
                         MUV.write v (offset+1) (colorConv c2)
                         MUV.write v (offset+twidth) (colorConv c3)
                         MUV.write v (offset+1+twidth) (colorConv c4)
                      )
                    )
                  return v
              )
            )


blitTile :: Surface -> Tile -> IO ()
blitTile surf (Tile (Rect xtmin ytmin twidth theight) pixels) =
  UV.zipWithM_ drawf (UV.fromList [0..((twidth*theight)-1)]) pixels
  where
    drawf i (r, g, b, a, d) = 
      setPixel32 surf (xtmin + (mod i twidth)) (ytmin + (div i twidth)) (rgbf (r*a) (g*a) (b*a))


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
  let tiles = runPar $ parMap (\block -> renderTileSubsample srect block scene) blocks
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

getTags' :: Int -> Int -> Scene -> [Tag]
getTags' x y scn =
  let (# xcoord, ycoord #) = getCoords xres yres x y
  in  
      get_tags xcoord ycoord scn

runHandler :: Scene -> IO ()
runHandler scn@(geom,_,_,_) = do
  e <- pollEvent
  --e <- waitEvent
  case e of
    Quit -> return ()
    MouseButtonUp x y ButtonLeft ->
      -- note: x and y are Word16, so we convert to plain Int
      do let ts = getTags' (fromIntegral x) (fromIntegral y) scn
         print $ (show x) ++ " " ++ (show y) ++ ":"
         forM_ ts print
         runHandler scn
    KeyDown (Keysym k modifiers unicode) ->
      case k of
       SDLK_q -> return ()
       SDLK_s -> do print $ (show geom)
                    runHandler scn
       _      -> runHandler scn
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
  scene@(geom, _, _, _) <- scn

  print $ "(primitives,transforms,bounding objects): " ++ (show (primcount geom))
  setupt2 <- getPOSIXTime
  print $ "scene setup: " ++ (show (setupt2-setupt1))

  -- Multiple renderings can be useful to see if rendering times are consistent.
  forM_ [1..1] (\_ ->
    do rendert1 <- getPOSIXTime
       renderTiles img scene blocksize
       rendert2 <- getPOSIXTime
       print $ "render: " ++ (show (rendert2-rendert1))
       
       blitt1 <- getPOSIXTime
       blitSurface img Nothing screen Nothing
       SDL.flip screen
       blitt2 <- getPOSIXTime
       print $ "blit: " ++ (show (blitt2-blitt1))
    )

  runHandler scene
