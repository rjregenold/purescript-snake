module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Timer (TIMER)
import Data.Array (deleteAt, elem, length, null, snoc, tail, (!!), (..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import DOM (DOM)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, Dimensions, fillRect, getCanvasDimensions, getCanvasElementById, getContext2D, setFillStyle)
import Signal (Signal, foldp, runSignal, sampleOn, (~>))
import Signal.DOM (keyPressed)
import Signal.Time (every, second)

gridSize :: Number
gridSize = 20.0

pieceSize :: Number
pieceSize = gridSize - 2.0

tileCount :: Int
tileCount = 20

data Position = Position
  { x :: Int
  , y :: Int
  }

instance eqPosition :: Eq Position where
  eq (Position a) (Position b) = a.x == b.x && a.y == b.y

data Velocity = Velocity
  { x :: Int
  , y :: Int
  }

type Player =
  { pos   :: Position
  , vel   :: Velocity
  , len   :: Int
  , trail :: Array Position
  }

initialPlayer :: Player
initialPlayer =
  { pos: Position { x: 10, y: 10 }
  , vel: Velocity { x: 0, y: 0 }
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

derive instance eqActiveInput :: Eq ActiveInput

data PlayState
  = NotStarted
  | Playing
  | GameOver

derive instance eqPlayState :: Eq PlayState

type GameState =
  { boardDim  :: Dimensions
  , input     :: ActiveInput
  , player    :: Player
  , apple     :: Apple
  , eating    :: Boolean
  , playState :: PlayState
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

velocity :: Position -> Velocity -> Position
velocity (Position p) (Velocity v) = Position { x: p.x + v.x, y: p.y + v.y }

intersects :: Position -> Position -> Boolean
intersects (Position a) (Position b) = a.x == b.x && a.y == b.y

wrap :: Int -> Int -> Int -> Int
wrap mn mx v | v < mn    = mx
             | v >= mx   = mn
             | otherwise = v

wrapPos :: Int -> Int -> Int -> Int -> Position -> Position
wrapPos mnx mxx mny mxy (Position p) = 
  Position { x: wrap mnx mxx p.x, y: wrap mny mxy p.y }

playerLogic :: GameState -> Player
playerLogic st = (wrapPlayer <<< playerVelocity <<< trimTrail <<< addTrail <<< eat st.eating <<< move st.input) st.player
  where
    move None p  = p
    move Left p  = p { vel = Velocity { x: -1, y: 0 } }
    move Up p    = p { vel = Velocity { x: 0, y: -1 } }
    move Right p = p { vel = Velocity { x: 1, y: 0 } }
    move Down p  = p { vel = Velocity { x: 0, y: 1 } }

    eat e p =
      if e
      then p { len = p.len + 1 }
      else p

    addTrail p = p { trail = snoc p.trail p.pos }

    trimTrail p | length p.trail > p.len = p { trail = fromMaybe [] (tail p.trail) }
                | otherwise              = p

    playerVelocity p = p { pos = velocity p.pos p.vel }

    wrapPlayer p = p { pos = wrapPos 0 tileCount 0 tileCount p.pos }

player :: GameState -> GameState
player = checkGameOver <<< updatePlayer
  where
    updatePlayer st = st { player = playerLogic st }

    checkGameOver st = 
      if st.player.pos `elem` st.player.trail
      then st { playState = GameOver }
      else st

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

activeInput :: forall eff. Eff (dom :: DOM | eff) (Signal ActiveInput)
activeInput = do
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
    drawBoard ctx' = do
      void $ setFillStyle "black" ctx'
      void $ fillRect ctx' { x: 0.0
                          , y: 0.0
                          , w: st.boardDim.width
                          , h: st.boardDim.height 
                          }

    drawApple ctx' a = 
      case getApplePos a of
        Just (Position pos) -> do
          void $ setFillStyle "red" ctx'
          void $ fillRect ctx' { x: (toNumber pos.x) * gridSize
                               , y: (toNumber pos.y) * gridSize
                               , w: pieceSize
                               , h: pieceSize
                               }
        Nothing  -> pure unit

    drawPlayer ctx' p@{ pos: (Position curPos) } = do
      void $ setFillStyle "#00cc00" ctx'
      for_ p.trail $ \(Position pos) -> do
        void $ fillRect ctx' { x: (toNumber pos.x) * gridSize
                             , y: (toNumber pos.y) * gridSize
                             , w: pieceSize
                             , h: pieceSize
                             }
      void $ setFillStyle playerHeadColor ctx'
      void $ fillRect ctx' { x: (toNumber curPos.x) * gridSize
                           , y: (toNumber curPos.y) * gridSize
                           , w: pieceSize
                           , h: pieceSize
                           }

    playerHeadColor =
      if st.playState == GameOver
      then "#ffa500"
      else "#00ff00"

allPos :: Array Position
allPos = do
  x <- 0..(tileCount - 1)
  y <- 0..(tileCount - 1)
  pure (Position { x: x, y: y })

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
gameLogic input st = 
  case st.playState of
    NotStarted ->
      if input /= None
      then updateInput input (st { playState = Playing })
      else st
    Playing ->
      (player <<< apple <<< eating <<< updateInput input) st
    GameOver -> 
      st

gameState :: forall e. Dimensions -> Eff (dom :: DOM, random :: RANDOM | e) (Signal GameState)
gameState dim = do
  input <- activeInput
  pool <- shuffle allPos
  pure (foldp gameLogic (initialGameState pool) (sampleOn frameRate input))
  where
    initialGameState pool =
      { boardDim: dim
      , input: None
      , player: initialPlayer
      , apple: { pool: pool
               , idx: 0 
               } 
      , eating: false
      , playState: NotStarted
      }

runGame :: forall e. CanvasElement -> Eff (AppEff e) Unit
runGame canvas = do
  log "starting game"
  dim <- getCanvasDimensions canvas
  ctx <- getContext2D canvas
  game <- gameState dim
  runSignal (game ~> render ctx)

main :: forall e. Eff (AppEff e) Unit
main = do
  mCanvas <- getCanvasElementById "app"
  log "in main"
  maybe (log "missing canvas") runGame mCanvas
