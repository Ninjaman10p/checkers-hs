{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, TupleSections #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.BChan
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Time.Clock
import System.Environment

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.List

import Control.Monad
import Control.Monad.State.Class
import Control.Concurrent
import Control.Lens
import Control.Arrow

data Color = White | Black
  deriving (Eq, Ord, Show)

data Rank = Pawn | King
  deriving (Eq, Ord, Show)

data VertDir = North | South
  deriving (Eq, Ord, Show)

data HorDir = East | West
  deriving (Eq, Ord, Show)

type MoveDir = (VertDir, HorDir)

allDirs :: [MoveDir]
allDirs = (,) <$> [North, South] <*> [East, West]

data MoveType = Move | Capture
  deriving (Eq, Ord, Show)

data Checker = Checker { _team :: Color, _rank :: Rank }
  deriving (Show, Ord, Eq)
$(makeLenses ''Checker)

data Tick = Tick UTCTime

data Checkers = Checkers
  { _board :: M.Map (Int, Int) Checker
  , _currentTurn :: Color
  , _cursor :: (Int, Int)
  , _currentTime :: UTCTime
  , _lastBlink :: UTCTime
  , _blinkOn :: Bool
  , _possibleMoves :: M.Map ((Int, Int), MoveDir) MoveType
  , _moving :: Maybe (Int, Int)
  , _victor :: Maybe Color
  , _forceCapture :: Bool
  }
$(makeLenses ''Checkers)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  if listContains "--help" args
    then putStrLn $ helpMessage progName
    else startCheckers $ toggleableVal True "force-capture" args

listContains :: Eq a => a -> [a] -> Bool
listContains a = foldr ((||) . (== a)) False

helpMessage :: String -> String
helpMessage progName = unlines
  [ "Usage: " <> progName <> " [OPTIONS]"
  , ""
  , "Options:"
  , "  --help: Print this message"
  , "  --no-force-capture: disable force capture"
  , "  --force-capture: enable force capture (default)"
  ]
  
toggleableVal :: Bool -> String -> [String] -> Bool
toggleableVal defVal name = foldl' (flip updateVal) defVal
  where updateVal a | a == "--" <> name = const True
                    | a == "--no-" <> name = const False
                    | otherwise = id

startCheckers :: Bool -> IO ()
startCheckers force = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan <- newBChan 10
  void . forkIO . forever $ do
    time <- getCurrentTime
    threadDelay 100000
    writeBChan chan $ Tick time
  time <- getCurrentTime
  let rows = fmap ((,) White) [0..2] <> fmap ((,) Black) [5..7]
  void $ customMain initialVty builder (Just chan) checkersApp $
    foldr ((.) . uncurry createRow) id rows $
      Checkers { _board = M.empty
               , _currentTurn = Black
               , _cursor = (0, 6)
               , _currentTime = time
               , _lastBlink = time
               , _blinkOn = True
               , _possibleMoves = M.empty
               , _moving = Nothing
               , _victor = Nothing
               , _forceCapture = force
               }

allowedDirs :: Checker -> S.Set MoveDir
allowedDirs (Checker _ King) = S.fromList $ allDirs
allowedDirs (Checker Black Pawn) = S.fromList $ (North,) <$> [East, West]
allowedDirs (Checker White Pawn) = S.fromList $ (South,) <$> [East, West]

handleEvent :: BrickEvent () Tick -> EventM () Checkers ()
handleEvent (AppEvent (Tick time)) = do
  modify $ set currentTime time
  blink <- view lastBlink <$> get
  if diffUTCTime time blink > blinkInterval
    then do
      modify $ over blinkOn not
      modify $ set lastBlink time
    else return ()
handleEvent (VtyEvent (V.EvKey (V.KChar c) ms)) = handleKey c ms
handleEvent _ = return ()

handleKey :: Char -> [V.Modifier] -> EventM () Checkers ()
handleKey 'c' [V.MCtrl] = halt
handleKey 'j' [] = moveCursor _2 1
handleKey 'k' [] = moveCursor _2 (-1)
handleKey 'l' [] = moveCursor _1 1
handleKey 'h' [] = moveCursor _1 (-1)
handleKey 'd' [] = movePiece (North, East)
handleKey 'c' [] = movePiece (South, East)
handleKey 'a' [] = movePiece (North, West)
handleKey 'z' [] = movePiece (South, West)
handleKey _ _ = return ()

blinkInterval :: NominalDiffTime
blinkInterval = 0.8

checkersApp :: App Checkers Tick ()
checkersApp = App
          { appDraw = return . drawCheckers
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = calculateMoves
          , appAttrMap = mkAttrMap
          }

movePiece :: MonadState Checkers m => MoveDir -> m ()
movePiece m = do
  pos <- view cursor <$> get
  moveType <- view (possibleMoves . at (pos, m)) <$> get
  piece <- view (board . at pos) <$> get
  case piece of
    Just c -> do
      pos' <- case moveType of
        Just Move -> do
          let newPos = moveOffset m pos
          modify $ set (board . at newPos) (Just c)
          modify $ set (board . at pos) Nothing
          setCursorPos newPos
          swapTurn
          return newPos
        Just Capture -> do
          let capturePos = moveOffset m pos
              newPos = moveOffset m capturePos
          modify $ set (board . at newPos) (Just c)
          modify $ set (board . at capturePos) Nothing
          modify $ set (board . at pos) Nothing
          setCursorPos newPos
          modify $ set moving (Just newPos)
          return newPos
        _ -> return pos
      when (view _2 pos' == upgradeRow (view team c)) $
        modify $ set (board . at pos' . _Just . rank) King
    Nothing -> return ()
  calculateMoves
  
status :: Checkers -> String
status s = " " <> c <> " " 
  where c = case view victor s of
          Just Black -> "Player 1 Wins"
          Just White -> "Player 2 Wins"
          Nothing -> case view currentTurn s of
                       Black -> "Player 1 Turn"
                       White -> "Player 2 Turn"

swapTurn :: MonadState Checkers m => m ()
swapTurn =
  modify $ over currentTurn turnOver

calculateMoves :: MonadState Checkers m => m ()
calculateMoves = do
  boardState <- view board <$> get
  player <- view currentTurn <$> get
  s <- get
  mov <- view moving <$> get
  modify $ set possibleMoves $
    foldr (M.union) M.empty $ do
      mt <- allDirs
      return $
        M.mapKeys (, mt) $
        M.filter (\a -> (not $ isJust mov) || a == Capture) $
        M.mapMaybeWithKey (return . checkMove s mt) $
        M.filterWithKey (\k _ -> k == fromMaybe k mov) $
        M.filter ((== player) . view team) $
        M.filter (S.member mt . allowedDirs) $
          boardState
  force <- view forceCapture <$> get
  when force $ do
    newPossible <- view possibleMoves <$> get
    when (M.foldr ((||) . (== Capture)) False newPossible) $ do
      modify $ over possibleMoves $ M.filter (== Capture)
  newPossible' <- view possibleMoves <$> get
  when (M.null newPossible') $
    if isJust mov
      then do
        modify $ set moving Nothing
        swapTurn
        calculateMoves
      else
        modify $ set victor $ Just (turnOver player)

checkMove :: Checkers
          -> MoveDir
          -> (Int, Int)
          -> Maybe MoveType
checkMove s m pos
  | not . inbounds $ opos = Nothing
  | not . M.member opos $ boardState = Just Move
  | (view team <$> view (at opos) boardState) == Just (turnOver active)
  && view (at oopos) boardState == Nothing
  && inbounds oopos = Just Capture
  | otherwise = Nothing
  where boardState = view board s
        oopos = moveOffset m opos
        opos = moveOffset m pos
        active = view currentTurn s

inbounds :: (Int, Int) -> Bool
inbounds = uncurry (&&) <<< p *** p
  where p = uncurry (&&) <<< (< boardSize) &&& (>= 0)

switchDir :: MoveDir -> MoveDir
switchDir = switchVert *** switchHor

switchVert :: VertDir -> VertDir
switchVert North = South
switchVert South = North

switchHor :: HorDir -> HorDir
switchHor East = West
switchHor West = East

moveOffset :: MoveDir -> (Int, Int) -> (Int, Int)
moveOffset = uncurry (.) <<< vertOffset *** horOffset

vertOffset :: VertDir -> (Int, Int) -> (Int, Int)
vertOffset North = over _2 (+ (-1))
vertOffset South = over _2 (+1)

horOffset :: HorDir -> (Int, Int) -> (Int, Int)
horOffset East = over _1 (+1)
horOffset West = over _1 (+ (-1))

moveCursor :: MonadState Checkers m => Lens' (Int, Int) Int -> Int -> m ()
moveCursor l n = do
  pos <- view cursor <$> get
  let pos' = over l (+n) pos
  setCursorPos pos'

setCursorPos :: MonadState Checkers m => (Int, Int) -> m ()
setCursorPos p = do
  let bounder = max 0 . min (boardSize - 1)
      p' = (bounder *** bounder) $ p
  modify $ set cursor p'
  modify $ set blinkOn True
  time <- view currentTime <$> get
  modify $ set lastBlink time

turnOver :: Color -> Color
turnOver White = Black
turnOver Black = White

boardSize :: Int
boardSize = 8

boardOrientation :: Int
boardOrientation = 1

drawCheckers :: Checkers -> Widget ()
drawCheckers = C.center <<<
    flip (borderWithLabel . str . status)
      =<< hBox . (sequence $ makeColumn <$> range boardSize)

makeColumn :: Int -> Checkers -> Widget ()
makeColumn x = vBox . (sequence $ makeSquare x <$> range boardSize)

range :: Int -> [Int]
range n = [0..n - 1]

upgradeRow :: Color -> Int
upgradeRow Black = 0
upgradeRow White = 7

createRow :: Color -> Int -> Checkers -> Checkers
createRow col y = foldr (.) id $ range 4 <&> \n ->
  set (board . at (2 * n + mod (1 + y + boardOrientation) 2, y)) $ Just $
    Checker {_team = col, _rank = Pawn}

makeSquare :: Int -> Int -> Checkers -> Widget ()
makeSquare x y s =
  withAttr (squareAttr x y s) $
    if mod (x + y) 2 == boardOrientation
      then cell ' '
      else case view (board . at (x, y)) s of
            Just piece -> drawPiece piece
            Nothing -> cell ' '

squareAttr :: Int -> Int -> Checkers -> AttrName
squareAttr = bgAttr <> fgAttr

bgAttr :: Int -> Int -> Checkers -> AttrName
bgAttr x y s
  | pos == (x, y) && blink = attrName "selected"
  | not blink && (S.member pos $ checkValidMove (x, y) s) = attrName "valid-move"
  | mod (x + y) 2 == boardOrientation = attrName "white"
  | otherwise = mempty
  where blink = view blinkOn s
        pos = view cursor s

fgAttr :: Int -> Int -> Checkers -> AttrName
fgAttr x y = fromMaybe mempty . fmap (teamAttr . view team) . view (board . at (x, y))

checkValidMove :: (Int, Int) -> Checkers -> S.Set (Int, Int)
checkValidMove pos s = foldr (S.union) S.empty $ do
    dir <- allDirs
    let opos = moveOffset (switchDir dir) pos
        oopos = moveOffset (switchDir dir) opos
    return $ S.fromList $ do
      (p, movetype) <- [(opos, Move), (oopos, Capture)]
      if view (at (p, dir)) moves == Just movetype
        then [p]
        else []
  where moves = view possibleMoves s

drawPiece :: Checker -> Widget ()
drawPiece = cell . rankChar . view rank

teamAttr :: Color -> AttrName
teamAttr White = attrName "team-w"
teamAttr Black = attrName "team-b"

rankChar :: Rank -> Char
rankChar Pawn = '●'
-- rankChar King = 'ö'
-- rankChar King = '⁕'
rankChar King = '♦'

cell :: Char -> Widget ()
cell c = str $ " " <> (c : " ")

mkAttrMap :: Checkers -> AttrMap
mkAttrMap _ = attrMap (fg V.white) $ do
  (bgName, f) <- [ (attrName "selected", withBackColor V.blue)
                 , (attrName "white", withBackColor V.white)
                 , (attrName "valid-move", withBackColor V.yellow)
                 , (mempty, id)
                 ]
  (fgName, g) <- [ (attrName "team-b", withForeColor V.brightRed)
                 , (attrName "team-w", withForeColor V.brightWhite)
                 , (mempty, id)
                 ]
  return (bgName <> fgName, f . g $ fg V.white)

withStyle :: V.Style -> V.Attr -> V.Attr
withStyle = flip V.withStyle

withBackColor :: V.Color -> V.Attr -> V.Attr
withBackColor = flip V.withBackColor

withForeColor :: V.Color -> V.Attr -> V.Attr
withForeColor = flip V.withForeColor
