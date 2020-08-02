module Redraw where

import Xlib 
import Utilities

may_redraw :: Bool -> XInfo -> GameState -> IO ()
may_redraw ok xinfo state = if ok then redraw xinfo state else return ()

redraw :: XInfo -> GameState -> IO ()

redraw xinfo state = 
  let (XInfo display window gcontext gcontext2 gcontextp) = xinfo
  in
  xDrawRectangle (XDrawWindow window) gcontext2 (XRect 0 0 900 600) True >>
  drawBoard xinfo >>
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 610 65) "Player 1" >>
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 620 125) "Clock 1" >>
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 610 205) "Player 2" >>
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 620 265) "Clock 2" >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 45 130 30) False >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 105 90 30) False >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 185 130 30) False >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 245 90 30) False >>
  button 700 330 "New players"  xinfo >>
  button 700 360 "New game"  xinfo >>
  button 700 390 "Undo" xinfo >>
  button 700 420 "Load" xinfo >>
  button 700 450 "Save"  xinfo >>
  button 700 480 "Quit" xinfo >>
  helpButton xinfo >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 615 535 250 30) False >>
  let (GameState player1 player2 board steps weight1 weight2 time
                 numbersteps promptString next_player) = state
  in
  xMArrayLookup time 0 >>= \ lstm0 ->
  xMArrayLookup time 1 >>= \ lstm1 ->
  showtime 705 270 (lstm1) xinfo >>
  showtime 705 130 (lstm0) xinfo >>
  xMArrayLookup player1 0 >>= \ player1_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 65) player1_name >>
  xMArrayLookup player2 0 >>= \ player2_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 205) player2_name >>
  xMArrayLookup promptString 0 >>= \ ps ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 620 550) ps >>
  xMArrayLookup next_player 0 >>= \ next_player_num ->
  (if (next_player_num == 1)
   then xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 70) '<' 
   else xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 210) '<')
  >>
  drawPieces 1 1 board xinfo >>
  return ()  

drawHelp (XInfo display window gcontext gcontext2 gcontextp) = 
  xDrawRectangle (XDrawWindow window) gcontext2 (XRect 100 100 300 200) True >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 100 100 300 200) False >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 102 102 296 196) False >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 200 230 100 60) False >>
  xDrawRectangle (XDrawWindow window) gcontext (XRect 202 232 96 56) False >>
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 240 265) "OK" >>
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 120)
              "Two players in turn place black and white" >>
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 135)
              "pieces on the board. The winner is the" >>
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 150)
              "player who first makes five consecutive" >>
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 165)
              "pieces in either vertical, horizontal or" >>
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 180)
              "diagonal directions." >>
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 200)
              "To play with a robot, type \"computer\" as" >>
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 215)
              "the name of another player."


drawBoard (XInfo display window gcontext gcontext2 gcontextp) =
  drawvlines 30 30 1 >>
  drawhlines 30 30 1 >>  
  drawmarks where

  drawvlines :: Int -> Int -> Int -> IO ()
  drawvlines x y z 
                | z <= 19 
                   = xDrawLine (XDrawWindow window) gcontext
                     (XPoint x y) (XPoint x (y+30*18)) >>  
		       drawvlines (x+30) y (z+1)
                | otherwise
                   = return ()

  drawhlines :: Int -> Int -> Int -> IO ()
  drawhlines x y z 
                | z <= 19
                   = xDrawLine (XDrawWindow window) gcontext
                     (XPoint x y) (XPoint (x+30*18) y) >> 
                       drawhlines x (y+30) (z+1)
                | otherwise 
                   = return ()

  drawmarks :: IO ()
  drawmarks =
            map2IO (\x y ->
                     xDrawArc (XDrawWindow window) gcontext 
                              (XArc x y 6 6 (-1.0) 6.283) True)
                   (map (\x -> 30 + x*30-3) [3,9,15,3,9,15,3,9,15])
                   (map (\x -> 30 + x*30-3) [3,3,3,9,9,9,15,15,15])
            >> return ()

map2IO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]

map2IO f [] []         = return []
map2IO f (x:xs) (z:zs) = f x z >>= \ y -> 
		         map2IO f xs zs >>= \ ys -> 
		         return (y:ys)

drawPieces 20 _ board xinfo = return ()
drawPieces x 20 board xinfo = drawPieces (x+1) 1 board xinfo
drawPieces x y board xinfo = 
  xMArrayLookup board ((x-1)*19 + y-1) >>= \ piece ->
  (if (piece == 1 || piece == 2)
   then drawPiece x y xinfo (piece == 1)
   else return ()) >>
  drawPieces x (y+1) board xinfo
  
drawPiece x y (XInfo display window gcontext gcontext2 _ ) is_black =
  (if is_black then return ()
               else xDrawArc (XDrawWindow window) gcontext2 
                             (XArc (30*x-10) (30*y-10) 20 20
                             (-1.0) 6.283)
                             True) >> 
  xDrawArc (XDrawWindow window) gcontext 
           (XArc (30*x-10) (30*y-10) 20 20
  	   (-1.0) 6.283)
           is_black >>
  xDisplayForceOutput display
