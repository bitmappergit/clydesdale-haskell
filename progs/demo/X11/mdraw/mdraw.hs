module MDraw where

import Xlib 

mapIO :: (a -> IO b) -> [a] -> IO [b]

mapIO f []     = return []
mapIO f (x:xs) = f x >>= \ y -> 
                 mapIO f xs >>= \ ys -> 
		 return (y:ys)

map2IO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]

map2IO f [] []         = return []
map2IO f (x:xs) (z:zs) = f x z >>= \ y -> 
		         map2IO f xs zs >>= \ ys -> 
		         return (y:ys)

xGetEventMul              :: XMArray XDisplay -> IO (Int, XEvent)
xGetEventMul displays = 
  let n_displays = xMArrayLength displays
      loop :: Int -> IO (Int, XEvent)
      loop i = if i == n_displays then loop 0
               else xMArrayLookup displays i >>= \ display ->
                    xDisplayForceOutput display >>
                    xEventListen display >>= \ n_events ->
                    if n_events == 0 then loop (i + 1)
                    else xGetEvent display >>= \ event ->
                         return (i, event)
  in loop 0

-- takes a list of host names

mdraw :: [String] -> IO ()
mdraw hosts =
  mapIO xOpenDisplay hosts >>= \ displays ->
  let screens = map (head . xDisplayRoots) displays
      fg_colors = map xScreenBlackPixel screens
      bg_colors = map xScreenWhitePixel screens
      roots = map xScreenRoot screens
  in
  map2IO (\ root color -> 
              xCreateWindow root 
                            (XRect 100 100 400 400)
                            [XWinBackground color,
		             XWinEventMask (XEventMask [XButtonMotion, 
                                                        XButtonPress])])
         roots
         bg_colors 
  >>= \windows ->
  mapIO xMapWindow windows >>
  map2IO xCreateGcontext 
        (map XDrawWindow roots) 
        (map (\ color -> [XGCForeground color]) fg_colors)
  >>= \ gcontexts ->
  xMArrayCreate displays >>= \ displayArr ->
  let
    handleEvent lasts =
      xGetEventMul displayArr >>= \ (idx, event) ->
        let pos = xEventPos event
	in
	case (xEventType event) of
          XButtonPressEvent  -> 
            xMArrayUpdate lasts idx pos >>
            handleEvent lasts
          XMotionNotifyEvent ->
            xMArrayLookup lasts idx >>= \ last -> 
            map2IO (\ window gcontext -> xDrawLine (XDrawWindow window) 
                                                    gcontext 
                                                    last 
                                                    pos)
                   windows
                   gcontexts
            >>
            xMArrayUpdate lasts idx pos >>
            handleEvent lasts
          _                  -> handleEvent lasts
  in
  xMArrayCreate (map (\ _ -> XPoint 0 0) hosts) >>= \ lasts ->
  handleEvent lasts >>
  return ()

