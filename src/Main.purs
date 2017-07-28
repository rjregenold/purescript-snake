module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Timer (TIMER)
import Data.Array (length, replicate, snoc, tail, (:), (!!))
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

playerLogic :: ActiveInput -> Player -> Player
playerLogic input p = (wrapPlayer <<< velocity <<< trimTrail <<< addTrail <<< move input) p
  where
    move None p  = p
    move Left p  = p { vel = { x: -1.0, y: 0.0 } }
    move Up p    = p { vel = { x: 0.0, y: -1.0 } }
    move Right p = p { vel = { x: 1.0, y: 0.0 } }
    move Down p  = p { vel = { x: 0.0, y: 1.0 } }

    -- TODO: do not hardcode board size here
    wrapPlayer p = p { pos = { x: wrap 0.0 19.0 p.pos.x, y: wrap 0.0 19.0 p.pos.y } }

    addTrail :: Player -> Player
    addTrail p = p { trail = snoc p.trail p.pos }

    trimTrail :: Player -> Player
    trimTrail p | length p.trail > p.len = p { trail = fromMaybe [] (tail p.trail) }
                | otherwise              = p

player :: Signal ActiveInput -> Signal Player
player input = foldp playerLogic initialPlayer (sampleOn frameRate input)

getApplePos :: Apple -> Maybe Position
getApplePos a = a.pool !! a.idx

nextApplePos :: Apple -> Apple
nextApplePos a =
  let idx' = a.idx + 1
  in  if idx' >= length a.pool
      then a { idx = 0 }
      else a { idx = idx' }

appleLogic :: Player -> Apple -> Apple
appleLogic p a = 
  case getApplePos a of
    Just pos -> if intersects p.pos pos
                then nextApplePos a
                else a
    Nothing  -> a

apple :: Array Position -> Signal Player -> Signal Apple
apple pool = foldp appleLogic initialApple
  where
    initialApple = { pool: pool, idx: 0 }

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

render :: forall eff. Context2D -> (Tuple Player Apple) -> Eff (canvas :: CANVAS | eff) Unit
render ctx (Tuple p a) = do
  drawBoard ctx
  drawApple ctx a
  drawPlayer ctx p
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

runGame :: forall eff. CanvasElement -> Eff (AppEff eff) Unit
runGame canvas = do
  log "starting game"
  dim <- getCanvasDimensions canvas
  ctx <- getContext2D canvas
  activeInput <- input
  randPool <- sequence (replicate 400 (randPos 0 19))
  let player' = player activeInput
      apple'  = apple randPool player'
      scene   = Tuple <~ player' ~ apple'
  runSignal (scene ~> render ctx)

main :: forall eff. Eff (AppEff eff) Unit
main = do
  mCanvas <- getCanvasElementById "app"
  log "in main"
  maybe (log "missing canvas") runGame mCanvas











{--runGame :: forall eff. CanvasElement -> Eff (AppEff eff) Unit--}
{--runGame canvas = do--}
  {--log "in game"--}
  {--dim <- getCanvasDimensions canvas--}
  {--ctx <- getContext2D canvas--}
  {--let st = {boardDim: dim, player: initialPlayer, apple: {x: 15.0, y: 15.0}, stepNum: 0}--}
  {--inputs <- mkInputs--}
  {--let frames = every second--}
  {--let game = foldp gameLogic (pure st) frames -- (sampleOn frames inputs)--}
  {--runSignal (render ctx <$> game)--}

  {--where--}
    {--mkInputs = do--}
      {--leftInputs <- keyPressed leftKeyCode--}
      {--upInputs <- keyPressed upKeyCode--}
      {--rightInputs <- keyPressed rightKeyCode--}
      {--downInputs <- keyPressed downKeyCode--}
      {--pure ({ left: _, up: _, right: _, down: _ } <$> leftInputs <*> upInputs <*> rightInputs <*> downInputs)--}

    {--gameLogic :: forall eff. Inputs -> Eff (random :: RANDOM | eff) GameState -> Eff (random :: RANDOM | eff) GameState--}
    {--gameLogic inputs gameState = do--}
      {--st <- appleLogic =<< gameState--}
      {--pure (st { stepNum = st.stepNum + 1, player = playerLogic inputs st.player })--}

    {--appleLogic :: forall eff. GameState -> Eff (random :: RANDOM, console :: CONSOLE | eff) GameState--}
    {--appleLogic st = do--}
      {--ifM (isEatingApple st.apple st.player)--}
        {--(newApplePos >>= (\pos -> pure (st { player = st.player { len = st.player.len + 1 }, apple = pos })))--}
        {--(pure st)--}

    {--isEatingApple :: forall eff. Position -> Player -> Eff (console :: CONSOLE | eff) Boolean--}
    {--isEatingApple apple player = do--}
      {--[>log ("apple: " <> (show apple.x) <> ", " <> (show apple.y))<]--}
      {--[>log ("player: " <> (show player.pos.x) <> ", " <> (show player.pos.y))<]--}
      {--pure (apple.x == player.pos.x && apple.y == player.pos.y)--}

    {--newApplePos :: forall eff. Eff (random :: RANDOM, console :: CONSOLE | eff) Position--}
    {--newApplePos = do--}
      {--[>log "getting new apple pos"<]--}
      {--newX <- randomInt 0 (floor (tileCount - 1.0))--}
      {--newY <- randomInt 0 (floor (tileCount - 1.0))--}
      {--pure { x: toNumber newX, y: toNumber newY }--}

    {--playerLogic :: Inputs -> Player -> Player--}
    {--playerLogic inputs = trimTrail <<< updateTrail <<< updatePlayerPosition <<< move inputs.left inputs.up inputs.right inputs.down--}

    {--move true _ _ _ p = p { vel = {x: -1.0, y: 0.0} }--}
    {--move _ true _ _ p = p { vel = {x: 0.0, y: -1.0} }--}
    {--move _ _ true _ p = p { vel = {x: 1.0, y: 0.0} }--}
    {--move _ _ _ true p = p { vel = {x: 0.0, y: 1.0} }--}
    {--move _ _ _ _ p = p--}

    {--updatePlayerPosition p = p { pos = clampPlayerPosition { x: p.pos.x + p.vel.x, y: p.pos.y + p.vel.y } }--}

    {--clampPlayerPosition :: Position -> Position--}
    {--clampPlayerPosition pos | pos.x < 0.0        = pos { x = tileCount }--}
                            {--| pos.x >= tileCount = pos { x = 0.0 }--}
                            {--| pos.y < 0.0        = pos { y = tileCount }--}
                            {--| pos.y >= tileCount = pos { y = 0.0 }--}
                            {--| otherwise          = pos--}

    {--updateTrail :: Player -> Player--}
    {--updateTrail p = p { trail = snoc p.trail p.pos }--}

    {--trimTrail :: Player -> Player--}
    {--trimTrail p | length p.trail > p.len = p { trail = fromMaybe [] (tail p.trail) }--}
                {--| otherwise              = p--}

    {--render ctx st = do--}
      {--st' <- st--}
      {--drawBoard ctx st'--}
      {--drawApple ctx st'--}
      {--drawPlayer ctx st'--}

    {--drawBoard ctx st = do--}
      {--ctx' <- setFillStyle "black" ctx--}
      {--void $ fillRect ctx' {x: 0.0, y: 0.0, w: st.boardDim.width, h: st.boardDim.height}--}

    {--drawApple ctx st = do--}
      {--ctx' <- setFillStyle "red" ctx--}
      {--void $ fillRect ctx' {x: st.apple.x * gridSize, y: st.apple.y * gridSize, w: gridSize - 2.0, h: gridSize - 2.0}--}

    {--drawPlayer ctx st = do--}
      {--ctx' <- setFillStyle "lime" ctx--}
      {--for_ st.player.trail $ \pos -> do--}
        {--void $ fillRect ctx' {x: pos.x * gridSize, y: pos.y * gridSize, w: gridSize - 2.0, h: gridSize - 2.0}--}
