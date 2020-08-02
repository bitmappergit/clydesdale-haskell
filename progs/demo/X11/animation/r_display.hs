module R_Display (displaym) where

import R_Ptypes
import R_Utility
import Xlib
import R_Constants

displaym :: String -> Int -> Movie -> IO ()

displaym host n movie =
  let
    movie' = cycle (take n (map (map translatePoly) movie))
  in
  xOpenDisplay host 
  >>= \ display ->
  let (screen:_) = xDisplayRoots display
      fg_color = xScreenBlackPixel screen
      bg_color = xScreenWhitePixel screen
      color_map = xScreenDefaultColormap screen
      getPixels [] = return []
      getPixels (c:cs) = 
        xLookupColor color_map c >>= \ (xc, _) ->
     	xAllocColor color_map xc >>= \ (p,_,_) ->
        getPixels cs >>= \ ps ->
        return (p:ps) 
  in
  getPixels (map colorName allColors) 
  >>= \ pixels ->
  let
    lookupPixel c = lookupPixel1 c allColors pixels

    lookupPixel1 x []     _      = head pixels
    lookupPixel1 x (c:cs) (p:ps) = 
      if x == c then p
                else lookupPixel1  x cs ps
    parent = xScreenRoot screen
  in
  xMArrayCreate [lookupPixel i | i <- [0..15]] 
  >>= \ pixelArray ->
  xCreateGcontext (XDrawWindow parent)
                  [XGCBackground bg_color,
                   XGCForeground fg_color]
  >>= \ gcontext ->
  xCreateGcontext (XDrawWindow parent)
                  [XGCBackground bg_color,
                   XGCForeground bg_color] 
  >>= \ blank_gcontext ->
  xCreateWindow parent
                (XRect 100 100 500 500)
                [XWinBackground bg_color,
                 XWinEventMask (XEventMask [XButtonPress])] 
  >>= \window ->
  let depth = xDrawableDepth (XDrawWindow window) 
  in
  xCreatePixmap (XSize 500 500) depth (XDrawWindow parent)
  >>= \ pixmap ->
  xMapWindow window 
  >>= \() ->
  let
    dispFrame m = 
      xDrawRectangle (XDrawPixmap pixmap) 
                     blank_gcontext 
		     (XRect 0 0 500 500) 
		     True 
      >>
      dispPic m 
      >>
      xCopyArea (XDrawPixmap pixmap) gcontext (XRect 0 0 500 500) 
                (XDrawWindow window) (XPoint 0 0) 
      >>
      xDisplayForceOutput display

    dispPic [] = return ()
    dispPic (p:ps) = dispPoly p >> dispPic ps

    dispPoly (c, vec) =
--      xLookupColor color_map (colorName c) >>= \ ec ->
--      xAllocColor color_map ec >>= \ p -> 
      xMArrayLookup pixelArray c >>= \p ->
      xUpdateGcontext gcontext [XGCForeground p] >>= \ () ->
--      xSetGcontextForeground gcontext (lookupPixel c) >>= \ () ->
      xDrawLines (XDrawPixmap pixmap) gcontext vec True

    untilButton3 (frame:frames) = 
      let 
        action = dispFrame frame >> untilButton3 frames
      in
      xEventListen display >>= \count ->
      if count == 0 then action else
      xGetEvent display >>= \event ->
        case (xEventType event) of
	 XButtonPressEvent -> 
	   case (xEventCode event) of
	     3 -> return ()
	     _ -> action
         _                       -> action
  in
  putStr ("Click right button to end.\n") >>
  untilButton3 movie' >>
  xFreePixmap pixmap >>
  xCloseDisplay display

type Movie' = [Pic']
type Pic' = [Poly']
type Poly' = (Int, [XPoint])

translatePoly :: Poly -> Poly'
translatePoly (c, vs) = (c, flatten_2 vs)

flatten_2 []        = []
flatten_2 ((a,b):r) = (XPoint (a `div` 2) (b `div` 2)):(flatten_2 r)

