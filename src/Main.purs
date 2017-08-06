module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Timer (TIMER)
import Data.Array (deleteAt, elem, length, null, replicate, snoc, tail, (!!), (..))
import Data.Foldable (foldr, for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import DOM (DOM)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, Dimensions)
import Graphics.Canvas as Canvas
import Signal (Signal, foldp, runSignal, sampleOn, (~>))
import Signal.DOM (keyPressed)
import Signal.Time (every, second)


-- | The number of tiles on the game board.
tileCount :: Int
tileCount = 20

-- | The pixel size of each tile on the game board.
tileSize :: Number
tileSize = 20.0

-- | The size of a piece on the game board.
pieceSize :: Number
pieceSize = tileSize - 2.0

-- | A position on the game board.
data Position = Position
  { x :: Int
  , y :: Int
  }

instance eqPosition :: Eq Position where
  eq (Position a) (Position b) = a.x == b.x && a.y == b.y

-- | Velocity type.
data Velocity = Velocity
  { x :: Int
  , y :: Int
  }

-- | The player.
type Player =
  { pos   :: Position
  , vel   :: Velocity
  , dir   :: Maybe Direction
  , len   :: Int
  , trail :: Array Position
  }

-- | Initial state for the player.
initialPlayer :: Player
initialPlayer =
  { pos: Position { x: (tileCount / 2), y: (tileCount / 2) }
  , vel: Velocity { x: 0, y: 0 }
  , dir: Nothing
  , len: 3
  , trail: []
  }

-- | The apple. It contains a pool which should be a shuffled Array of all valid 
-- | positions and an index which points to the current apple position in the
-- | pool.
type Apple =
  { pool :: Array Position
  , idx  :: Int
  }

-- | The valid inputs for the game.
data ActiveInput
  = None
  | LeftArrow
  | UpArrow
  | RightArrow
  | DownArrow

derive instance eqActiveInput :: Eq ActiveInput

-- | The valid directions for the game.
data Direction
  = DirLeft
  | DirRight
  | DirUp
  | DirDown

derive instance eqDirection :: Eq Direction

-- | The valid states for the game.
data PlayState
  = NotStarted
  | Playing
  | GameOver

derive instance eqPlayState :: Eq PlayState

-- | The game state.
type GameState =
  { boardDim  :: Dimensions
  , input     :: ActiveInput
  , player    :: Player
  , apple     :: Apple
  , eating    :: Boolean
  , playState :: PlayState
  }

-- | The effects needed for this app.
type AppEff eff =
  ( console :: CONSOLE
  , canvas  :: CANVAS
  , dom     :: DOM
  , timer   :: TIMER
  , random  :: RANDOM
  | eff
  )

-- | Alias for color.
type Color = String

-- | The player trail color.
playerTrailColor :: Color
playerTrailColor = "#00cc00"

-- | The player head color during normal gameplay.
playerHeadColor :: Color
playerHeadColor = "#00ff00"

-- | The player head color when the game is over.
playerHeadColorFail :: Color
playerHeadColorFail = "#ffa500"

-- | Defines the frame rate signal.
frameRate :: Signal Number
frameRate = every (second / 15.0)

-- | Applies velocity to a position.
velocity :: Position -> Velocity -> Position
velocity (Position p) (Velocity v) = Position { x: p.x + v.x, y: p.y + v.y }

-- | Tests for an intersection between two points.
intersects :: Position -> Position -> Boolean
intersects (Position a) (Position b) = a.x == b.x && a.y == b.y

-- | Ensures the given value is inside the bounds.
wrap :: Int -> Int -> Int -> Int
wrap mn mx v | v < mn    = mx
             | v >= mx   = mn
             | otherwise = v

-- | Given bounds, ensures the given position stays within them.
wrapPos :: Int -> Int -> Int -> Int -> Position -> Position
wrapPos mnx mxx mny mxy (Position p) = 
  Position { x: wrap mnx mxx p.x, y: wrap mny mxy p.y }

-- | Returns a collection of all possible game positions.
allPos :: Array Position
allPos = do
  x <- 0..(tileCount - 1)
  y <- 0..(tileCount - 1)
  pure (Position { x: x, y: y })

-- | Shuffles a collection using the Fisher-Yates shuffle.
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

-- | Gets the next player step from the current game state.
playerLogic :: GameState -> Player
playerLogic st = 
  (wrapPlayer 
   <<< playerVelocity 
   <<< trimTrail 
   <<< addTrail 
   <<< eat st.eating 
   <<< move st.input) st.player
  where
    -- move checks the previous direction to ensure the player does not
    -- turn back onto themselves (thus ending the game)
    move None p  = p

    move LeftArrow p | p.dir == (Just DirRight) = p
                     | otherwise = p { vel = Velocity { x: -1, y: 0 }, dir = Just DirLeft }

    move UpArrow p | p.dir == (Just DirDown) = p
                   | otherwise = p { vel = Velocity { x: 0, y: -1 }, dir = Just DirUp }

    move RightArrow p | p.dir == (Just DirLeft) = p
                      | otherwise = p { vel = Velocity { x: 1, y: 0 }, dir = Just DirRight }

    move DownArrow p | p.dir == (Just DirUp) = p
                     | otherwise = p { vel = Velocity { x: 0, y: 1 }, dir = Just DirDown }

    eat e p =
      if e
      then p { len = p.len + 1 }
      else p

    addTrail p = p { trail = snoc p.trail p.pos }

    trimTrail p | length p.trail > p.len = p { trail = fromMaybe [] (tail p.trail) }
                | otherwise              = p

    playerVelocity p = p { pos = velocity p.pos p.vel }

    wrapPlayer p = p { pos = wrapPos 0 tileCount 0 tileCount p.pos }

-- | Handles the player logic (including checks for game over).
player :: GameState -> GameState
player = checkGameOver <<< updatePlayer
  where
    updatePlayer st = st { player = playerLogic st }

    checkGameOver st = 
      if st.player.pos `elem` st.player.trail
      then st { playState = GameOver }
      else st

-- | Gets the position of an apple
getApplePos :: Apple -> Maybe Position
getApplePos a = a.pool !! a.idx

-- | Gets the next apple position
nextApplePos :: Player -> Apple -> Apple
nextApplePos p = checkPlacement p <<< moveNext
  where
    moveNext a' = 
      let idx = a'.idx + 1
      in  if idx >= length a'.pool
          then a' { idx = 0 }
          else a' { idx = idx }

    -- make sure we are not placing the apple under the player
    checkPlacement p' a' = 
      case getApplePos a' of
        Just pos -> 
          if pos == p'.pos || pos `elem` p'.trail
          then nextApplePos p' a'
          else a'
        Nothing -> a'

-- | Handles the apple logic.
apple :: GameState -> GameState
apple st = 
  if st.eating
  then st { apple = nextApplePos st.player st.apple }
  else st

-- | Inits the input signal.
activeInput :: forall e. Eff (dom :: DOM | e) (Signal ActiveInput)
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

    toInput true _ _ _ = LeftArrow
    toInput _ true _ _ = UpArrow
    toInput _ _ true _ = RightArrow
    toInput _ _ _ true = DownArrow
    toInput _ _ _ _    = None

-- | Given the current GameState, determines if the player is eating an apple.
eating :: GameState -> GameState
eating st = st { eating = maybe false (intersects st.player.pos) (getApplePos st.apple) }

-- | Updates the GameState with the current input.
updateInput :: ActiveInput -> GameState -> GameState
updateInput input st = st { input = input }

-- | Changes the play state to Playing.
startPlaying :: GameState -> GameState
startPlaying st = st { playState = Playing }

-- | Executes the game logic to produce the next game state.
gameLogic :: ActiveInput -> GameState -> GameState
gameLogic input st = 
  case st.playState of
    NotStarted ->
      if input /= None
      then (step <<< startPlaying) st
      else st
    Playing ->
      step st
    GameOver -> 
      st
  where
    -- calculates the next step of the game state
    step = player <<< apple <<< eating <<< updateInput input

-- | Inits the game state signal.
gameState :: forall e. Dimensions -> Eff (dom :: DOM, random :: RANDOM | e) (Signal GameState)
gameState dim = do
  input <- activeInput
  pool <- shuffleTimes 10 allPos
  let p = initialPlayer
      -- creates an apple and sets its position
      a  = nextApplePos p { pool: pool, idx: 0 }
  pure (foldp gameLogic (initialGameState a p) (sampleOn frameRate input))
  where
    initialGameState a p =
      { boardDim: dim
      , input: None
      , player: p
      , apple: a 
      , eating: false
      , playState: NotStarted
      }

    -- executes the shuffle function n times.
    shuffleTimes :: forall e2. Int -> Array Position -> Eff (random :: RANDOM | e2) (Array Position)
    shuffleTimes n = foldr (<=<) pure (replicate n shuffle)

-- | Renders the game state.
render :: forall e. Context2D -> GameState -> Eff (canvas :: CANVAS | e) Unit
render ctx st = do
  renderGame
  renderOverlay
  where
    renderGame = do
      renderBoard
      renderApple st.apple
      renderPlayer st.player

    renderBoard = do
      void $ Canvas.setFillStyle "black" ctx
      void $ Canvas.fillRect ctx { x: 0.0
                                 , y: 0.0
                                 , w: st.boardDim.width
                                 , h: st.boardDim.height 
                                 }

    renderApple a =
      case getApplePos a of
        Just pos -> renderTile "red" pos
        Nothing  -> pure unit

    renderPlayer p = do
      renderTrail p.trail
      renderHead p.pos

    renderTrail ts = for_ ts (renderTile playerTrailColor)

    renderHead = renderTile headColor

    renderTile color (Position pos) = do
      void $ Canvas.setFillStyle color ctx
      void $ Canvas.fillRect ctx { x: (toNumber pos.x) * tileSize
                                 , y: (toNumber pos.y) * tileSize
                                 , w: pieceSize
                                 , h: pieceSize
                                 }

    headColor =
      if st.playState == GameOver
      then playerHeadColorFail
      else playerHeadColor

    renderOverlay = 
      case st.playState of
        NotStarted -> renderNotStarted
        GameOver   -> renderGameOver
        _          -> pure unit

    renderMsg msg = do
      void $ Canvas.setFont "20px sans-serif" ctx
      m <- Canvas.measureText ctx msg
      void $ Canvas.setFillStyle "white" ctx
      void $ Canvas.fillText ctx msg ((st.boardDim.width - m.width) / 2.0) 40.0

    renderNotStarted = renderMsg "Press the arrow keys to start"

    renderGameOver = renderMsg "GAME OVER :("

-- | Runs the game.
runGame :: forall e. CanvasElement -> Eff (AppEff e) Unit
runGame canvas = do
  dim <- Canvas.getCanvasDimensions canvas
  ctx <- Canvas.getContext2D canvas
  game <- gameState dim
  runSignal (game ~> render ctx)

-- | Entrypoint for the game.
main :: forall e. Eff (AppEff e) Unit
main = do
  mCanvas <- Canvas.getCanvasElementById "app"
  maybe (Console.error "missing canvas") runGame mCanvas
