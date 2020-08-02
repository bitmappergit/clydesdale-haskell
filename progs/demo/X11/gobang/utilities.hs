module Utilities where

import Xlib
import Weights
import Redraw
import Misc
 
data XInfo = XInfo XDisplay XWindow XGcontext XGcontext XGcontext
data GameState = GameState (XMArray String) (XMArray String) (XMArray Int)
                           (XMArray Int) (XMArray Int) (XMArray Int) 
                           (XMArray Integer) (XMArray Int)
                           (XMArray String) (XMArray Int)

type GameCont = XInfo -> GameState -> IO ()

xMArrayToList :: XMArray a -> IO [a]
xMArrayToList a = 
   let la = xMArrayLength a
       loop i a = if i == la then return []
                  else xMArrayLookup a i >>= \ x ->
                       loop (i+1) a >>= \ xs ->
                       return (x:xs)
   in
   loop 0 a


readGameState str =
  let
    [(board_lst, r1)] = reads str
    [(weight1_lst, r2)] = reads r1
    [(weight2_lst, r3)] = reads r2
    [(steps_lst, r4)] = reads r3
    [(player1_lst, r5)] = reads r4
    [(player2_lst, r6)] = reads r5
    [(time_lst, r7)] = reads r6
    [(numbersteps_lst, r8)] = reads r7
    [(promptString_lst, r9)] = reads r8
    [(next_player_lst, [])] = reads r9
  in
  xMArrayCreate board_lst >>= \ board ->
  xMArrayCreate weight1_lst >>= \ weight1 ->
  xMArrayCreate weight2_lst >>= \ weight2 ->
  xMArrayCreate steps_lst >>= \ steps ->
  xMArrayCreate player1_lst >>= \ player1 ->
  xMArrayCreate player2_lst >>= \ player2 ->
  xMArrayCreate time_lst >>=  \ time ->
  xMArrayCreate numbersteps_lst >>= \ numbersteps ->
  xMArrayCreate promptString_lst >>= \ promptString ->
  xMArrayCreate next_player_lst >>= \ next_player ->
  return (GameState player1 player2 board steps weight1 weight2 time
                      numbersteps promptString next_player)

showGameState (GameState player1 player2 board steps weight1 weight2 time
                      numbersteps promptString next_player) =
  xMArrayToList board >>= \ board_lst ->
  xMArrayToList weight1 >>= \ weight1_lst ->
  xMArrayToList weight2 >>= \ weight2_lst ->
  xMArrayToList steps >>= \ steps_lst ->
  xMArrayToList player1 >>= \ player1_lst ->
  xMArrayToList player2 >>= \ player2_lst ->
  xMArrayToList time >>=  \ time_lst ->
  xMArrayToList numbersteps >>= \ numbersteps_lst ->
  xMArrayToList promptString >>= \ promptString_lst ->
  xMArrayToList next_player >>= \ next_player_lst ->
  let
    str =(shows board_lst .
          shows weight1_lst .
          shows weight2_lst .
          shows steps_lst .
          shows player1_lst .
          shows player2_lst .
          shows time_lst .
          shows numbersteps_lst .
          shows promptString_lst .
          shows next_player_lst) []
  in
  return str

                   
xMod      :: Int -> Int -> Int
xMod x y | x >= y      = xMod (x-y) y 
         | otherwise   = x

xRes      :: Int -> Int -> Int -> Int
xRes x y z | x >= y     = xRes (x-y) y (z+1) 
           | otherwise = z

drawCmd :: String -> XInfo -> GameState -> IO ()
drawCmd a (XInfo display window gcontext gcontext2 gcontextp)
          (GameState _ _ _ _ _ _ _ _ str _)
           = xDrawRectangle (XDrawWindow window) gcontext2
                (XRect 616 536 248 28) True >>= \ () ->
             xDrawGlyphs (XDrawWindow window) gcontext 
                         (XPoint 620 550) a  >>
             xMArrayUpdate str 0 a >>
             xDisplayForceOutput display

clearCmd :: XInfo -> GameState -> IO ()
clearCmd (XInfo display window gcontext gcontext2 gcontextp)
         (GameState _ _ _ _ _ _ _ _ str _)
          = xDrawRectangle (XDrawWindow window) gcontext2
                (XRect 616 536 248 28) True >>= \() ->
            xMArrayUpdate str 0 "" >>
            xDisplayForceOutput display 

xPosition :: Int -> XPoint
xPosition  a = (XPoint (xRes a 19 1) (1+ (xMod a 19)))

initArray :: XMArray a -> Int -> Int -> a -> IO ()
initArray mary x y z | x<y       = xMArrayUpdate mary x z >>= \() ->
                                   initArray mary (x+1) y z
                     | otherwise = return ()

getposition :: Int -> Int -> XMaybe (Int, Int)
getposition x y = let x1 = round ((fromIntegral x) / 30.0)
                      y1 = round ((fromIntegral y) / 30.0)
                  in
                  if (x1 < 1 || x1 > 19 || y1 < 1 || y1 > 19) then XNull
                  else XSome (x1, y1)

addZero :: Int -> String
addZero a | a < 10    = "0"
          | otherwise =  ""

printTime :: Int -> Int -> [Int] -> XInfo -> IO()
printTime x y zs (XInfo display window gcontext gcontext2 gcontextp)
           = let s = head zs
                 m = head (tail zs)
                 h = head (tail (tail zs))
             in  xDrawRectangle (XDrawWindow window) gcontext2 
                     (XRect (x-4) (y-24) 88 28) True >>= \() ->
                 xDrawGlyphs (XDrawWindow window) gcontextp (XPoint x y)
                    ((addZero h)++(show h)++":"++(addZero m)++(show m)++
                          ":"++(addZero s)++(show s)) >>
                 xDisplayForceOutput display

showtime :: Int -> Int -> Integer -> XInfo -> IO()
showtime x y z a = 
  let (curtm, c) = (decodeTime z (WestOfGMT 0))
  in  printTime x y curtm a

helpButton :: XInfo -> IO ()
helpButton (XInfo display window  gcontext gcontext2 gcontextp) = 
        xDrawRectangle (XDrawWindow window) gcontext (XRect 800 420 70 70)
                       False >>
        xDrawRectangle (XDrawWindow window) gcontext (XRect 802 422 66 66)
                       False >>
        xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 810 450) "About" 
        >>
        xDrawGlyphs (XDrawWindow window) gcontext (XPoint 820 470) "Gobang" 
        >>
        return ()

ishelp :: Int -> Int -> Bool
ishelp x y = (x > 800 && x < 870 && y > 420 && y < 490)

button :: Int -> Int -> String -> XInfo -> IO()
button x y a (XInfo display window  gcontext gcontext2 gcontextp) = 
        xDrawArc (XDrawWindow window) gcontext 
          (XArc (x-40) (y-10) 20 20 1.5708 4.7124) True  >>= \() ->
        xDrawRectangle (XDrawWindow window) gcontext 
          (XRect (x-30) (y-10) 60 20) True  >>= \() ->
        xDrawArc (XDrawWindow window) gcontext
          (XArc (x+20) (y-10) 20 20 (-1.0) 6.283) True >>= \() ->
        xDrawGlyphs (XDrawWindow window) gcontext2 
          (XPoint (x-(length a * 3)) (y+4)) a   >>
        xDisplayForceOutput display

-- a b are the location of the button, c d are the point where we press the
-- button.

buttonPress :: Int -> Int -> Int -> Int -> Bool
buttonPress a b c d | (abs (c-a))<=30 && (abs (d-b))<=10   = True
                    | (c-a+30)*(c-a+30)+(d-b)*(d-b)<=100   = True
                    | (c-a-30)*(c-a-30)+(d-b)*(d-b)<=100   = True
                    | otherwise                            = False



randmax :: XMArray Int -> Int -> Int -> [Int] -> IO Int
randmax a ind max mi | ind > 360  = 
                       let lmi = length mi
                       in case lmi of
                          0 -> return (-1)
                          1 -> return (head mi)
                          _ -> random lmi >>= \ i ->
                               return (mi !! i)
                     | otherwise  = xMArrayLookup a ind >>= \ tt3 ->
                                    if (tt3 > max) 
                                    then randmax a (ind+1) tt3 [ind]
                                    else if (tt3 == max) 
                                         then randmax a (ind+1) max (ind:mi)
                                         else randmax a (ind+1) max mi

robot :: XMArray Int -> XMArray Int -> XMArray Int -> IO XPoint
robot numbersteps weight1 weight2
      = xMArrayLookup numbersteps 0 >>= \(tt5) ->
        if (tt5 == 0)
           then return (XPoint 10 10)
           else
		randmax weight1 0 0 [] >>= \ tmp1 ->
		randmax weight2 0 0 [] >>= \ tmp2 ->
		xMArrayLookup weight1 tmp1 >>= \ tmp3 ->
                xMArrayLookup weight2 tmp2 >>= \ tmp4 ->
                  if (tmp3 >= 200) 
                      then return (xPosition tmp1)
                      else if (tmp3 > tmp4)
                               then return (xPosition tmp1)
                               else return (xPosition tmp2)


promptFor prompt xinfo state =
  let (GameState player1 player2 board steps weight1 weight2 time
                 numbersteps promptString next_player) = state
      (XInfo display window gcontext gcontext2 gcontextp) = xinfo
  in
  xDrawRectangle (XDrawWindow window) gcontext2
                 (XRect 616 536 248 28) True >>
  xMArrayUpdate promptString 0 prompt >> 
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 620 550) prompt >>
  xDisplayForceOutput display >>
  let h_base = (length prompt + 1) * 6 + 620
      getString :: Int -> String -> IO String
      getString h_pos sofar =
        xGetEvent display >>= \event ->
        case (xEventType event) of
          XButtonPressEvent -> 
            let (XPoint x y) = xEventPos event
            in 
            (if ishelp x y then helpGame xinfo state 
             else xBell display 0)
            >>
            getString h_pos sofar
          XExposureEvent -> 
            may_redraw (xEventCount event == 0) xinfo state >>
            xDrawGlyphs (XDrawWindow window) gcontext (XPoint h_base 550) sofar
            >>
            xDrawRectangle (XDrawWindow window) gcontext
                           (XRect (h_base + 6 * h_pos) (550-10) 6 13) True
            >> getString h_pos sofar
          XKeyPressEvent -> 
            let code = xEventCode event
                state = xEventState event
                bs = if (sofar == "") then getString h_pos sofar
                     else xDrawRectangle (XDrawWindow window) gcontext2 
                                         (XRect (h_base + 6 * h_pos) 
                                                (550-10) 6 13) 
                                         True >>
                          xDrawRectangle (XDrawWindow window) gcontext 
                                         (XRect (h_base + 6 * (h_pos - 1)) 
                                                (550-10) 6 13) 
                                         True >> 
                          getString (h_pos-1) (take (length sofar - 1) sofar) 
            in  
            xKeycodeCharacter display code state >>= \ char ->
            case char of
               (XSome '\r') -> return sofar
               (XSome '\DEL') -> bs
               (XSome '\BS') -> bs
               XNull     -> getString h_pos sofar
               (XSome c) -> xDrawRectangle (XDrawWindow window) gcontext2 
                                           (XRect (h_base + 6 * h_pos) 
                                                  (550-10) 6 13) 
                                           True >> 
                            xDrawGlyph (XDrawWindow window) gcontext
                                       (XPoint (h_base + 6 * h_pos) 550) c >>
                            xDrawRectangle (XDrawWindow window) gcontext 
                                           (XRect (h_base + 6 * (h_pos + 1)) 
                                                  (550-10) 6 13) 
                                           True >> 
                            getString (h_pos + 1) (sofar ++ [c])

  in 
  xDrawRectangle (XDrawWindow window) gcontext
                 (XRect h_base (550-10) 6 13) True >>
  getString 0 ""


helpGame xinfo@(XInfo display window gcontext gcontext2 gcontextp) state =
  drawHelp xinfo >>
  let
    loop xinfo state = 
      xGetEvent display >>= \ event ->
      case (xEventType event) of
        XExposureEvent -> may_redraw (xEventCount event == 0) xinfo state >>
                          drawHelp xinfo >>
                          loop xinfo state
        XButtonPressEvent -> 
                          let (XPoint x y) = xEventPos event
                          in
                          if (x > 200 && x < 300 && y > 230 && y < 290) 
                          then redraw xinfo state >> 
                               return ()
                          else loop xinfo state
        _              -> xBell display 0 >>
                          loop xinfo state
  in
  loop xinfo state



