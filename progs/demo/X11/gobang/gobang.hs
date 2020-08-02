module Gobang where

import Xlib
import Utilities
import Redraw
import Weights

getXInfo :: String -> IO XInfo
getXInfo host = 
  xOpenDisplay host >>= \ display ->
  let (screen:_) = xDisplayRoots display 
      fg_pixel = xScreenBlackPixel screen
      bg_pixel = xScreenWhitePixel screen
      root = xScreenRoot screen
  in 
  xCreateWindow root
                (XRect 0 0 900 600)
                [XWinBackground bg_pixel, 
                 XWinEventMask (XEventMask [XButtonPress, 
                                            XKeyPress, 
                                            XExposure])]
                 >>= \ window ->
  xSetWmName window "Gobang" >>
  xMapWindow window >>
  xOpenFont display "10x20" >>=  \ playerfont ->
  xOpenFont display "6x13" >>= \ genericfont ->
  xCreateGcontext (XDrawWindow window)
                  [XGCBackground bg_pixel,      
                   XGCForeground fg_pixel] >>= \ gcontext  ->
  xCreateGcontext (XDrawWindow window)
                  [XGCBackground fg_pixel,
                   XGCForeground bg_pixel,
                   XGCFont       genericfont] >>= \ gcontext2 ->
  xCreateGcontext (XDrawWindow window)
                  [XGCBackground bg_pixel,
                   XGCForeground fg_pixel,
                   XGCFont       playerfont] >>= \ gcontextp ->
  return (XInfo display window gcontext gcontext2 gcontextp)

demo = main

main = getEnv "DISPLAY" >>= \ host ->
       try (gobang host) (\ e -> putStr (showError e))

gobang :: String -> IO ()
gobang host =
  getXInfo host >>= \ xinfo ->
  xMArrayCreate [1..361] >>= \ board ->
  xMArrayCreate [1..361] >>= \ weight1 ->
  xMArrayCreate [1..361] >>= \ weight2 ->
  xMArrayCreate [1..722] >>= \ steps ->
  xMArrayCreate [""] >>= \ player1 ->
  xMArrayCreate [""] >>= \ player2 ->
  xMArrayCreate [1..4] >>=  \ time ->
  xMArrayCreate [1] >>= \ numbersteps ->
  xMArrayCreate [""] >>= \ promptString ->
  xMArrayCreate [1] >>= \ next_player ->
  let state = GameState player1 player2 board steps weight1 weight2 time
                        numbersteps promptString next_player
  in
  initGame xinfo state >>
  promptPlayers xinfo state >>
  playGame xinfo state

promptPlayers xinfo state = 
  let (XInfo display window gcontext gcontext2 gcontextp) = xinfo
      (GameState player1 player2 board steps weight1 weight2 time
                 numbersteps promptString next_player) = state
  in
  promptFor "player 1:" xinfo state >>= \ player1_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 65) player1_name >>
  xMArrayUpdate player1 0 player1_name >>
  promptFor "player 2:" xinfo state >>= \ player2_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 205) player2_name >>
  xMArrayUpdate player2 0 player2_name >>
  clearCmd xinfo state

initGame :: XInfo -> GameState -> IO ()
initGame xinfo 
         state@(GameState player1 player2 board steps weight1 weight2 time
                          numbersteps promptString next_player) =
          getTime >>= \ curtime ->
          initArray time 0 2 0 >>
          initArray time 2 4 curtime >>
          initArray numbersteps 0 1 0 >>
          initArray board 0 361 0 >>
          initArray weight1 0 361 0 >>
          initArray weight2 0 361 0 >>= \ () ->
          initArray next_player 0 1 1 >>= \ () ->
          clearCmd xinfo state >>= \ () ->
          redraw xinfo state
 

handleButton :: XPoint -> XInfo -> GameState -> GameCont -> IO ()
handleButton (XPoint x y) 
             xinfo
             state@(GameState player1 player2 board steps weight1 weight2 time
                              numbersteps promptString next_player)
             cont 
       | buttonPress 700 330 x y  = initArray player1 0 1 "" >>
                                    initArray player2 0 1 "" >>
                                    initGame xinfo state >>
                                    promptPlayers xinfo state >>
                                    playGame xinfo state
       | buttonPress 700 360 x y  = initGame xinfo state >>
                                    playGame xinfo state
       | buttonPress 700 390 x y  = undoGame xinfo state cont
       | buttonPress 700 420 x y  = loadGame xinfo state cont
       | buttonPress 700 450 x y  = saveGame xinfo state >>= \ () ->
                                    cont xinfo state
       | buttonPress 700 480 x y  = quitGame xinfo state cont
       | ishelp x y          = helpGame xinfo state >>= \ () ->
                               cont xinfo state
       | otherwise           = cont xinfo state

when :: Bool -> IO () -> IO ()
when cond action = if cond then action else return ()

undoGame xinfo@(XInfo display window gcontext gcontext2 gcontextp)
         state@(GameState player1 player2 board steps weight1 weight2 time
                          numbersteps promptString next_player)
         cont =
  xMArrayLookup next_player 0 >>= \ next_p ->
  xMArrayLookup player1 0 >>= \ name1 ->
  xMArrayLookup player2 0 >>= \ name2 ->
  let undoStep n =
        xMArrayLookup steps (2*n) >>= \ x ->
        xMArrayLookup steps (2*n+1) >>= \ y ->
        xMArrayUpdate board ((x-1)*19 + y-1) 0 >>
        (if (name1 == "computer" || name2 == "computer") 
            then draw_unit board weight1 weight2 x y 
            else return ()) >>
       xDrawRectangle (XDrawWindow window) gcontext2 
                      (XRect (x*30-15) (y*30-15) 30 30) True >>
--        drawBoard xinfo >>
--        drawPieces 1 1 board xinfo >>
        let x30 = x * 30
            y30 = y * 30
            c = XPoint x30 y30
            w = XPoint (x30-15) y30
            e = XPoint (x30+15) y30
            no = XPoint x30 (y30-15)
            s = XPoint x30 (y30+15)
            m = XArc (x30-3) (y30-3) 6 6 (-1.0) 6.283
        in
        when (x > 1) (xDrawLine (XDrawWindow window) gcontext w c) >>
        when (x < 19) (xDrawLine (XDrawWindow window) gcontext c e) >>
        when (y > 1) (xDrawLine (XDrawWindow window) gcontext no c) >>
        when (y < 19) (xDrawLine (XDrawWindow window) gcontext c s) >>
        when ((x `elem` [4,10,16]) && (y `elem` [4,10,16]))
             (xDrawArc (XDrawWindow window) gcontext m True) >>
        xDisplayForceOutput display >>
        xMArrayUpdate numbersteps 0 n >>
        xMArrayLookup next_player 0 >>= \ next_p ->
        xMArrayUpdate next_player 0 (if next_p == 1 then 2 else 1) 

      cur_name = if next_p == 1 then name1 else name2
      last_name = if next_p == 1 then name2 else name1
  in
  xMArrayLookup numbersteps 0 >>= \ n ->
  if n==0 then drawCmd "No more steps to undo!" xinfo state >>
               cont xinfo state
  else 
  if cur_name == "computer" then cont xinfo state
  else
  (undoStep (n-1) >>
   if (last_name == "computer" && n /= 1) then undoStep (n-2)
   else
   return ()) >>
  playGame xinfo state
    



promptFile xinfo state cont =
  promptFor "File name:" xinfo state >>= \ name ->
  try (readFile name >>= (\ content -> cont (XSome content)))
      (\ _ -> drawCmd ("Can't read file:" ++ name) xinfo state >>
	      cont XNull)
           

loadGame xinfo state cont =
  promptFile xinfo state $ \ file ->
  case file of
    XNull -> cont xinfo state
    XSome file_content ->
     readGameState file_content >>= \ new_state ->
     let (GameState _ _ _ _ _ _ time _ _ _) = new_state
     in
     getTime >>= \ curtime ->
     initArray time 2 4 curtime >>
     redraw xinfo new_state >>
     playGame xinfo new_state

saveGame :: XInfo -> GameState -> IO ()
saveGame xinfo state =
  promptFor "File name:" xinfo state >>= \ name ->
  showGameState state >>= \ str ->
  try (writeFile name str)
      (\ _ -> drawCmd ("Can't write file: " ++ name) xinfo state)

quitGame :: XInfo -> GameState -> GameCont -> IO ()
quitGame xinfo state cont =
  let (XInfo display window gcontext gcontext2 gcontextp) = xinfo
  in
  promptFor "Are you sure? (y/n)" xinfo state >>= \ reps ->
  if (reps == "y" || reps == "Y") then xCloseDisplay display
                                  else clearCmd xinfo state >>
                                       cont xinfo state

playGame :: XInfo -> GameState -> IO ()
playGame xinfo state =
     let             
        (XInfo display window gcontext gcontext2 gcontextp) = xinfo
        (GameState player1 player2 board steps weight1 weight2 time
                   numbersteps promptString next_player) = state
     in
     xMArrayLookup numbersteps 0 >>= \ x ->
     (\cont -> if x == 361 
               then drawCmd "It's a tie!" xinfo state >>
                    let loop xinfo state = waitButton xinfo state (\ _ -> loop)
                    in loop xinfo state
               else cont) $        
     xMArrayLookup next_player 0 >>= \ next_player_num ->
     getTime >>= \ curtime ->
     xMArrayLookup time 0 >>= \ lstm0 ->
     xMArrayLookup time 1 >>= \ lstm1 ->
     xMArrayLookup time 2 >>= \ lstm2 ->
     xMArrayLookup time 3 >>= \ lstm3 ->
     drawCmd ("Waiting for player # " ++ (show next_player_num)) xinfo state >>
     if (next_player_num == 1)
        then xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 70)
                   '<' >>= \(trash) ->
             xDrawRectangle (XDrawWindow window) gcontext2 
	                    (XRect 840 180 40 40) True >>
             xMArrayUpdate time 2 curtime >>
             xMArrayUpdate time 1 (lstm1+curtime-lstm3) >>
             showtime 705 270 (lstm1+curtime-lstm3) xinfo >>
             xMArrayLookup player1 0 >>= \ x ->
             if (x == "computer") 
                   then computerplay xinfo state
                   else humanplay xinfo state
        else xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 210)
                    '<' >>= \(trash) ->
             xDrawRectangle (XDrawWindow window) gcontext2 
	                    (XRect 840 40 40 40)  True >>
             xMArrayUpdate time 3 curtime >>
             xMArrayUpdate time 0 (lstm0+curtime-lstm2) >>
             showtime 705 130 (lstm0+curtime-lstm3) xinfo >>
             xMArrayLookup player2 0 >>= \ x ->
             if (x == "computer") 
                   then computerplay xinfo state
                   else humanplay xinfo state

waitButton xinfo@(XInfo display _ _ _ _) state cont = 
  let
    loop xinfo state = 
      xGetEvent display >>= \ event ->
      case (xEventType event) of
        XExposureEvent -> may_redraw (xEventCount event == 0) xinfo state 
                          >>
                          loop xinfo state
        XButtonPressEvent -> 
                          let pos = xEventPos event
                          in 
                          handleButton pos xinfo state (cont pos)
        _              -> xBell display 0 >>
                          loop xinfo state
  in
  loop xinfo state

updateboard :: XInfo -> GameState -> Int -> Int -> IO ()
updateboard xinfo state x y = 
            let (GameState player1 player2 board steps weight1 weight2 time
                           numbersteps promptString next_player) = state
                (XInfo display window gcontext gcontext2 gcontextp) = xinfo
            in
            xMArrayLookup next_player 0 >>= \ next_player_num ->
            xMArrayUpdate next_player 0 (if next_player_num == 1 then 2 else 1)
            >> 
            xMArrayLookup numbersteps 0 >>= \ z ->
            xMArrayUpdate numbersteps 0 (z+1) >>
            xMArrayUpdate steps (2*z) x >>
            xMArrayUpdate steps (2*z+1) y >>
            xMArrayLookup player1 0 >>= \ name1 ->
            xMArrayLookup player2 0 >>= \ name2 ->
            xMArrayUpdate board (19*(x-1)+y-1) next_player_num >>
            human_unit board x y >>= \ win ->
            if win 
            then drawCmd ("Player " ++ (show next_player_num) ++ " has won!")
                         xinfo state >>
                 let loop xinfo state = waitButton xinfo state (\ _ -> loop)
                 in loop xinfo state
            else if (name1 == "computer" || name2 == "computer")
                 then draw_unit board weight1 weight2 x y >>
                      xMArrayUpdate weight1 (19*(x-1)+y-1) (-1) >>
                      xMArrayUpdate weight2 (19*(x-1)+y-1) (-1) >>
                      playGame xinfo state
                 else playGame xinfo state

choice :: XPoint -> XInfo -> GameState -> IO ()
choice (XPoint x y) xinfo@(XInfo display _ _ _ _) state =
   let (GameState player1 player2 board steps weight1 weight2 time
                  numbersteps promptString next_player) = state
   in
   case (getposition x y) of
     XNull -> humanplay xinfo state
     XSome (x, y) -> 
       xMArrayLookup board (19*(x-1)+y-1) >>= \ z ->
       if (z>0)
       then xBell display 0 >>
            drawCmd "Wrong point, please re-enter" xinfo state >>
            humanplay xinfo state
       else xMArrayLookup next_player 0 >>= \ next_player_num ->
            drawPiece x y xinfo (next_player_num == 1) >>
            updateboard xinfo state x y

humanplay :: XInfo -> GameState -> IO ()
humanplay xinfo state =  waitButton xinfo state choice

computerplay :: XInfo -> GameState -> IO ()
computerplay xinfo@(XInfo display window gcontext gcontext2 gcontextp)
             state = 
    let process_events xinfo state cont =
          xEventListen display >>= \ n_event ->
          if n_event == 0 then cont xinfo state
          else xGetEvent display >>= \ event ->
               case (xEventType event) of
                 XButtonPressEvent -> 
                            handleButton (xEventPos event) xinfo state cont
                 XExposureEvent    -> 
                            may_redraw (xEventCount event == 0)
                                       xinfo state 
                            >>
                            process_events xinfo state cont
                 XKeyPressEvent    ->
                            process_events xinfo state cont
    in
    process_events xinfo state $ 
    \ xinfo@(XInfo display window gcontext gcontext2 gcontextp)              
      state@(GameState _ _ _ _ weight1 weight2 _ numbersteps _ next_player) ->
    robot numbersteps weight1 weight2 >>= \pt ->
    let (XPoint x y) = pt
    in 
    xMArrayLookup next_player 0 >>= \ next_player_num ->
    drawPiece x y xinfo (next_player_num == 1) >>
    updateboard xinfo state x y




