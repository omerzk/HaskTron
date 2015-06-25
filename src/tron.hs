module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as Text


data PlayerState = PlayerState {coordinates::[(Double,Double)], color :: Color, alive :: Bool, prevX::Double, prevY::Double, inptInd::Int}
data GameState = GameState {state::Bool, players::[PlayerState]}
type Input = [(Int, Int)]

speed::Double
speed = 4

--default : two players pre-set colors and pos
defaultGame::GameState
defaultGame = GameState{state = False,
players = [
PlayerState {coordinates = [(20,0)],
 color = teal,
  alive = True,
   prevX = speed,
    prevY = 0,
     inptInd = 0},

     PlayerState {coordinates = [(-20,0)],
      color = maroon,
       alive = True,
        prevX = -speed,
         prevY = 0,
          inptInd = 1}]}

-- The response funcion which creates a new game state based on the input signal thus continuing the game signal
update:: Input -> GameState -> GameState
update input game =
  if state game then GameState {state = (fst (input !! 2) == 0), players = players'} else if snd (input !! 2) == 1 then defaultGame else GameState {state = (input !! 3) == (-1, -1), players = players game} -- redundent and ugly
  where players' = zipWith (updatePlayer (players game)) input (players game)

--update a players status
updatePlayer::  [PlayerState] -> (Int, Int) -> PlayerState -> PlayerState
updatePlayer players (dx, dy) state
          |alive state  = state {coordinates = (px', py') : (coordinates state),
                                            alive = alive',
                                             prevX = prevX',
                                              prevY = prevY'}
                                              |otherwise = state
            where prevX' = detMove dx dy (prevX state)
                  prevY' = detMove dy dx  (prevY state)
                  px' = cycle (fst (head (coordinates state)) + prevX')
                  py' = cycle (snd (head (coordinates state)) + prevY')
                  alive' = not $ crossed (px',py')

                  crossed (x,y) = (any (\pState -> (x,y) `elem` (coordinates pState)) players)

                  detMove dw dz prev
                    |(dz == 0) && dw/= 0 && (realToFrac (signum dw) + signum(prev) /= 0) = (realToFrac dw) * speed
                    |(dz /= 0) && dw == 0 = 0
                    |otherwise = prev
                  cycle p
                    |abs p > 380 = -p
                    |otherwise = p

--draw a single player
playerForm:: PlayerState -> [Form]
playerForm pState = (map moveTrail points)
  where
  points = every drawPace (coordinates pState)
  drawPace = 4 -- 10 before
  moveTrail = (flip move) erm
  erm = if alive pState then  filled (color pState) $ ngon playerEdges playerRadius else filled red $ circle playerRadius
    where
    playerRadius = 10
    playerEdges = 6

--Renders all the Players
render :: GameState  -> (Int, Int) -> Element
render game (w, h)  =
  if state game then centeredCollage w h (back:(concatMap playerForm (players game))) else centeredCollage w h pauseScreen  -- ++ others-- ++ $map shotForm survivors --(\pState -> playerForm pState) survivors--do i even need the lambda?
    where back1 = gradient grad $ square 750
          back = group [back1, rotate 1.571 back1] --  move(-380,-380) (toForm (image w h "blueback.png"))
          grad = radial (300, 300) 20 (0, 0) 20 [(0, blue), (1, navy)]
          pauseScreen = [im, im3, (gradient (linear (0, 0) (100, 100) [(0, maroon), (1, teal)]) $ ngon 8 200), move (0, -125) (filled red $ oval 50 30), eyer, eyel, mouth, txt]
          txt =blank --move(0,300) $ toForm $ Text.text $Text.toText $unlines ["Space - Start/Pause"," "," ","Tab -Reset"] -- Text{textUTF8 = "Start = Down+Right, Reset = Tab, Pause = space", textColor = lime} -- , fontSize = 30, fontWeight =10, fontSlant = FontSlantOblique
          im = move(-400, -350) (toForm (image w h "tri.png"))
--make these blanks to remove pause face
  mouth = move(-40, 80) (toForm (image 100 100 "mouth.png"))
          im3 = blank -- move(-375, -175) (toForm (image 750 750 "bear.png"))
          eyer = move(25, -10) (toForm (image 80 80 "eyer.png"))
          eyel = move(-110, -10) (toForm (image 80 80 "eyes.png"))
-- i guess it's kinda stupid to compose a screen of several overlaying images... consider saving the image of the head/whole pause screen and rendering it as a single
--survivors = filter (\pState -> alive pState) (players game) endScreen
--eye = (filled white $oval 35 80)
--rotate 1 (move (70, 50) eye), rotate 2.3 (move (-70, 50) eye)

-- the game signal the signal which encapsulates every other signal in the game,
--  including a time signal to set a sample rate an input signal from obviuos reasons and a games signal which constructs all frames of th egame
gameSignal :: FRP.Helm.Signal GameState
gameSignal = foldp update defaultGame input
  where input = lift2 processKeys Keyboard.keysDown (Time.fps 40)
        processKeys keys t = [(x, y), (z, w), (s, s), (r, r)] --terrible function(bigass array)
          where
            [x, y, z, w, s, r] = map process controlKeys
            controlKeys = [(Keyboard.LeftKey, Keyboard.RightKey),
             (Keyboard.UpKey, Keyboard.DownKey),
              (Keyboard.AKey, Keyboard.DKey),
               (Keyboard.WKey, Keyboard.SKey),
               (Keyboard.SpaceKey, Keyboard.TabKey),
                (Keyboard.ReturnKey, Keyboard.ReturnKey)]
            process (key1, key2)
              |key1 `elem` keys  = -1
              |key2 `elem` keys = 1
              |otherwise = 0

-- instead of saving every (x,y) save every nth pair(add a counter and  a prevP = (x,y)(so the next coord would be correct) )
--  and for the other use a last coordinate field, where in the form func chain the field to the coordinates
--  THIS WOULD ELIMINATE THE WIGGLING MOTIONS--------------------------------------------------------------------------------
every n xs = case drop (n - 1) xs of
              (y:ys) -> y : every n ys
              [] -> []

main :: IO ()
main = do run config $ render <~ gameSignal ~~ Window.dimensions
    where config = defaultConfig {windowTitle = "HaskiTron" , windowDimensions = (750, 750)}
--------------------------------------------------------------------------------------------------------