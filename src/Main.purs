module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Timer (TIMER)
import Data.Array (deleteAt, length, null, replicate, snoc, tail, (:), (!!), (..))
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Debug.Trace
import DOM (DOM)
import Graphics.Canvas
import Signal
import Signal.DOM (animationFrame, keyPressed)
import Signal.Time

gridSize :: Number
gridSize = 20.0

tileCount :: Number
tileCount = 20.0

type Position =
  { x :: Number
  , y :: Number 
  }

type Velocity =
  { x :: Number
  , y :: Number
  }

type Player =
  { pos   :: Position
  , vel   :: Velocity
  , len   :: Int
  , trail :: Array Position
  }

initialPlayer :: Player
initialPlayer =
  { pos: {x: 10.0, y: 10.0}
  , vel: {x: 0.0, y: 0.0}
  , len: 3
  , trail: []
  }

type Apple =
  { pool :: Array Position
  , idx  :: Int
  }

data ActiveInput
  = None
  | Left
  | Up
  | Right
  | Down

type GameState =
  { boardDim :: Dimensions
  , input    :: ActiveInput
  , player   :: Player
  , apple    :: Apple
  , eating   :: Boolean
  }

type AppEff eff =
  ( console :: CONSOLE
  , canvas  :: CANVAS
  , dom     :: DOM
  , timer   :: TIMER
  , random  :: RANDOM
  | eff
  )

frameRate :: Signal Number
frameRate = every (second / 15.0)

velocity :: Player -> Player
velocity p = p { pos = { x: p.pos.x + p.vel.x
                       , y: p.pos.y + p.vel.y
                       }
               }

intersects :: Position -> Position -> Boolean
intersects a b = a.x == b.x && a.y == b.y

wrap :: Number -> Number -> Number -> Number
wrap mn mx v | v < mn    = mx
             | v > mx    = mn
             | otherwise = v

playerLogic :: GameState -> Player
playerLogic st = (wrapPlayer <<< velocity <<< trimTrail <<< addTrail <<< eat st.eating <<< move st.input) st.player
  where
    move None p  = p
    move Left p  = p { vel = { x: -1.0, y: 0.0 } }
    move Up p    = p { vel = { x: 0.0, y: -1.0 } }
    move Right p = p { vel = { x: 1.0, y: 0.0 } }
    move Down p  = p { vel = { x: 0.0, y: 1.0 } }

    eat e p =
      if e
      then p { len = p.len + 1 }
      else p

    addTrail p = p { trail = snoc p.trail p.pos }

    trimTrail p | length p.trail > p.len = p { trail = fromMaybe [] (tail p.trail) }
                | otherwise              = p

    -- TODO: do not hardcode board size here
    wrapPlayer p = p { pos = { x: wrap 0.0 19.0 p.pos.x, y: wrap 0.0 19.0 p.pos.y } }

player :: GameState -> GameState
player st = st { player = playerLogic st }

getApplePos :: Apple -> Maybe Position
getApplePos a = a.pool !! a.idx

nextApplePos :: Apple -> Apple
nextApplePos a =
  let idx' = a.idx + 1
  in  if idx' >= length a.pool
      then a { idx = 0 }
      else a { idx = idx' }

apple :: GameState -> GameState
apple st = 
  if st.eating
  then st { apple = nextApplePos st.apple }
  else st

input :: forall eff. Eff (dom :: DOM | eff) (Signal ActiveInput)
input = do
  left <- keyPressed leftKeyCode
  up <- keyPressed upKeyCode
  right <- keyPressed rightKeyCode
  down <- keyPressed downKeyCode
  pure (toInput <$> left <*> up <*> right <*> down)
  where
    leftKeyCode  = 37
    upKeyCode    = 38
    rightKeyCode = 39
    downKeyCode  = 40

    toInput true _ _ _ = Left
    toInput _ true _ _ = Up
    toInput _ _ true _ = Right
    toInput _ _ _ true = Down
    toInput _ _ _ _    = None

render :: forall e. Context2D -> GameState -> Eff (canvas :: CANVAS | e) Unit
render ctx st = do
  drawBoard ctx
  drawApple ctx st.apple
  drawPlayer ctx st.player
  where
    drawBoard ctx = do
      void $ setFillStyle "black" ctx
      -- TODO: no hard coding size
      void $ fillRect ctx { x: 0.0, y: 0.0, w: 400.0, h: 400.0 }

    drawApple ctx a = 
      case getApplePos a of
        Just pos -> do
          void $ setFillStyle "red" ctx
          void $ fillRect ctx {x: pos.x * gridSize, y: pos.y * gridSize, w: gridSize - 2.0, h: gridSize - 2.0}
        Nothing  -> pure unit

    drawPlayer ctx player = do
      void $ setFillStyle "lime" ctx
      for_ player.trail $ \pos -> do
        void $ fillRect ctx {x: pos.x * gridSize, y: pos.y * gridSize, w: gridSize - 2.0, h: gridSize - 2.0}

randPos :: forall e. Int -> Int -> Eff (random :: RANDOM | e) Position
randPos mn mx = do
  x <- randomInt mn mx
  y <- randomInt mn mx
  pure { x: toNumber x, y: toNumber y }

allPos :: Array Position
allPos = do
  x <- 0..19
  y <- 0..19
  pure { x: toNumber x, y: toNumber y }

shuffle :: forall a e. Array a -> Eff (random :: RANDOM | e) (Array a)
shuffle = go <<< Tuple []
  where
    go (Tuple acc xs) =
      if null xs
      then pure acc
      else do
        idx <- randomInt 0 (length xs - 1)
        go (fromMaybe (Tuple [] []) (f acc <$> xs !! idx <*> deleteAt idx xs))

    f acc x = Tuple (snoc acc x)

eating :: GameState -> GameState
eating st = st { eating = maybe false (intersects st.player.pos) (getApplePos st.apple) }

updateInput :: ActiveInput -> GameState -> GameState
updateInput input st = st { input = input }

gameLogic :: ActiveInput -> GameState -> GameState
gameLogic input = (player <<< apple <<< eating <<< updateInput input)

gameState :: forall e. Dimensions -> Eff (dom :: DOM, random :: RANDOM | e) (Signal GameState)
gameState dim = do
  activeInput <- input
  {--pool <- sequence (replicate 400 (randPos 0 19))--}
  pool <- shuffle allPos
  pure (foldp gameLogic (initialGameState pool) (sampleOn frameRate activeInput))
  where
    initialGameState pool =
      { boardDim: dim
      , input: None
      , player: initialPlayer
      , apple: { pool: pool
               , idx: 0 
               } 
      , eating: false
      }

runGame :: forall eff. CanvasElement -> Eff (AppEff eff) Unit
runGame canvas = do
  log "starting game"
  dim <- getCanvasDimensions canvas
  ctx <- getContext2D canvas
  game <- gameState dim
  runSignal (game ~> render ctx)

main :: forall eff. Eff (AppEff eff) Unit
main = do
  mCanvas <- getCanvasElementById "app"
  log "in main"
  maybe (log "missing canvas") runGame mCanvas
