module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as Text

data PlayerState = PlayerState {coordinates::[(Double,Double)], color :: Color, alive :: Bool, prevX::Double, prevY::Double, inptInd::Int}
data GameState = GameState {state::Bool, players::[PlayerState]}
type Input = [(Int,Int)]--TODO: this is where single vs multi comes into play
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


update:: Input -> GameState -> GameState
update input game = if state game then GameState {state = (fst (input !! 2) == 0), players = players'} else  if snd (input !! 2)  <= 0 then GameState {state = (input !! 2)  ==(-1,-1) , players = players game} else defaultGame -- redundent and ugly
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
          back = group [back1,rotate 1.571 back1]
          grad = radial (0,0) 20 (100,100) 20 [(0, blue),(1, navy)]
          pauseScreen = [im,(gradient (linear (0,0) (100,100) [(0, maroon),(1, teal)])  $ ngon 8 200),move (0, -125) (filled red $ oval 50 30),rotate 1 (move (70, 60) eye), rotate 2.3 (move (-70, 60) eye)]
          txt =blank -- toForm $ Text.text $Text.toText $unlines ["Start - Down+Right"," ","Reset - Tab"," ", "Pause - space"] -- Text{textUTF8 = "Start = Down+Right, Reset = Tab, Pause = space", textColor = lime} -- , fontSize = 30, fontWeight =10, fontSlant = FontSlantOblique
          im = move(-300,-350) (toForm (fittedImage 750 750 "wings.png"))
          eye = (filled white $oval 35 80)
          --          survivors = filter (\pState -> alive pState) (players game) endScreen


gameSignal :: FRP.Helm.Signal GameState
gameSignal = foldp update defaultGame input
  where input = lift2 processKeys Keyboard.keysDown (Time.fps 40)
        processKeys keys t = [(x,y), (z,w),(s,s)] --terrible function
          where
            [x, y, z, w, s] = map process controlKeys
            controlKeys = [(Keyboard.LeftKey, Keyboard.RightKey),(Keyboard.UpKey,Keyboard.DownKey),(Keyboard.AKey,Keyboard.DKey),(Keyboard.WKey ,Keyboard.SKey),(Keyboard.SpaceKey, Keyboard.TabKey)]
            process (key1, key2)
              |key1 `elem` keys  = -1
              |key2 `elem` keys = 1
              |otherwise = 0

-- TODO: this is crap, instead of saving every (x,y) save every nth pair and for the other use a last coordinate field, where in the form func chain the field to the coordinates -- THIS WOULD ELIMINATE THE WIGGLING MOTIONS
every n xs = case drop (n - 1) xs of
              (y:ys) -> y : every n ys
              [] -> []

main :: IO ()
main = do run config $ render <~ gameSignal ~~ Window.dimensions
    where config = defaultConfig {windowTitle = "HaskiTron" , windowDimensions = (750, 750)}
--------------------------------------------------------------------------------------------------------