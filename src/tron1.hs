module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Time as Time
data PlayerState = PlayerState {coordinates::[(Double,Double)], color :: Color, alive :: Bool, prevX::Double, prevY::Double, inptInd::Int}--shooting::Bool, coolDown::Int, sx::Int, sy:: Int
data GameState = GameState{state::Bool ,players::[PlayerState]}
type Input = [(Int,Int)]--TODO: this is where single vs multi comes into play

defaultGame::GameState
defaultGame = GameState{state = False, players = [PlayerState {coordinates = [(20,0)], color = teal, alive = True, prevX = -20, prevY = 0, inptInd = 0} ,PlayerState {coordinates = [(-20,0)], color = maroon, alive = True, prevX = 20, prevY = 0, inptInd = 1}]}

-- A list of colors for multiplayer
pallete :: [Color]
pallete = [red, lime, blue, yellow, cyan, magenta, maroon, navy, green, teal, purple]

update:: Input -> GameState -> GameState
update input game = GameState {state = True, players = players'}
  where players' = zipWith (updatePlayer (players game)) input (players game)

updatePlayer::  [PlayerState] -> (Int, Int) -> PlayerState -> PlayerState
updatePlayer players (dx, dy) state
          |alive state  = state {coordinates = (px', py') : (coordinates state),
                                            alive = alive',
                                             prevX = prevX',
                                              prevY = prevY'}
                                              |otherwise = state
            where dx'
                    |(dy == 0) && (dx /= 0) && (realToFrac (signum dx)  + signum(prevX state) /= 0) = realToFrac (dx * 20)
                    |(dy /= 0) && dx == 0 = 0
                    |otherwise = prevX state
                  dy'
                    |(dx == 0) && dy/= 0 && (realToFrac (signum dy) + signum(prevY state) /= 0) = realToFrac (dy * 20)
                    |(dx /= 0) && dy == 0 = 0
                    |otherwise = prevY state
                  px' = fst (head (coordinates state)) + dx' -- TODO: cosemtics only consider dropping
                  py' = snd (head (coordinates state)) + dy' -- TODO: cosemtics only consider dropping
                  prevX' = dx'
                  prevY' = dy'
                  alive' = not $ crossed (px',py')
                    where crossed (x,y) = (any (\pState -> (x,y) `elem` (coordinates pState)) players)

-------------------------------------------------------------------------------------------------------------------
playerForm:: PlayerState -> [Form]
playerForm pState = (map moveTrail (coordinates pState)) -- :message --playerForm pState =  traced (solid red) (path (coordinates pState))
  where
  moveTrail = (flip move) erm
  --message = if alive pState then [] else  Text{ textUTF8 = "Game Over", textColor = magenta, textHeight = 50, textWeight = 100}
  erm = if alive pState then  filled (color pState) $ ngon playerEdges playerRadius else filled red $ circle playerRadius
    where
    playerRadius = 10
    playerEdges = 6


--Renders all the Players
render :: GameState  -> (Int, Int) -> Element
render game (w, h)  =
  centeredCollage w h $ concatMap playerForm (players game) -- ++ others-- ++ $map shotForm survivors --(\pState -> playerForm pState) survivors--do i even need the lambda?
    where survivors = filter (\pState -> alive pState) (players game)
          others = [move (40,2) $filled (red) $ circle 10]

gameSignal :: FRP.Helm.Signal GameState
gameSignal = foldp update defaultGame input
  where input = lift processKeys Keyboard.keysDown

processKeys keys = [(x,y), (z,w)] --terrible function
  where
    [x,y,z,w] = map process controlKeys
    controlKeys = [(Keyboard.LeftKey, Keyboard.RightKey),(Keyboard.UpKey,Keyboard.DownKey),(Keyboard.AKey,Keyboard.DKey),(Keyboard.WKey ,Keyboard.SKey)]
    process (key1, key2)
      |key1 `elem` keys  = -1
      |key2 `elem` keys = 1
      |otherwise = 0

main :: IO ()
main = do run config $ render <~ gameSignal ~~ Window.dimensions
    where
    config = defaultConfig { windowTitle = "HaskTron" , windowDimensions = (750, 750)}

--------------------------------------------------------------------------------------------------------