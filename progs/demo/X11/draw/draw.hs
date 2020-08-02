module Draw where

import Xlib 

main = getEnv "DISPLAY" >>= (\ host -> draw host)

draw :: String -> IO ()
draw host =
  xOpenDisplay host >>= \ display ->
  let (screen:_) = xDisplayRoots display
      fg_color = xScreenBlackPixel screen
      bg_color = xScreenWhitePixel screen
      root = xScreenRoot screen
  in
  xCreateWindow root
                (XRect 100 100 400 400)
                [XWinBackground bg_color,
                 XWinEventMask (XEventMask [XButtonMotion, 
		                            XButtonPress,
                                            XKeyPress])] 
  >>= \window ->
  xMapWindow window >>= \() ->
  xCreateGcontext (XDrawWindow root)
                  [XGCBackground bg_color,
                   XGCForeground fg_color] >>= \ gcontext ->
  let
    handleEvent :: XPoint -> IO ()
    handleEvent last =
      xGetEvent display >>= \event ->
        let pos = xEventPos event
	in        
	case (xEventType event) of
          XButtonPressEvent  -> handleEvent pos
          XMotionNotifyEvent -> 
            xDrawLine (XDrawWindow window) gcontext last pos >>= \() ->
	    handleEvent pos
          XKeyPressEvent     -> xCloseDisplay display
          _                  -> handleEvent last
  in
  putStr "Press any key to quit.\n" >>
  handleEvent (XPoint 0 0)
